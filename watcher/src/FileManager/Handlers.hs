{-# LANGUAGE ScopedTypeVariables #-}

module FileManager.Handlers
  ( createDirectory
  , createFile
  , findFile
  , goToDirectory
  , fileContent
  , information
  , directoryContent
  , writeToFile
  , removeFileOrDirectory
  , debugFS
  )
  where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.List(intercalate)
import Data.Either (lefts, rights)
import qualified Data.Map.Strict as Map
import FileManager.FilePathUtils
import FileManager.FileSystemTypes
import System.FilePath ((</>))
import System.FilePath.Posix (isAbsolute, joinPath, splitFileName)

type ExceptState a = ExceptT FSException (State FSState) a

debugFS :: FilePath -> ExceptT FSException (State FSState) FilePath
debugFS _ = do
  FSState{fileSystem = fs, curDirectoryPath = curPath}<- get
  return $ "PATH TO CUR DIR : " ++  curPath ++ "\n" ++ "ROOTDIR" ++ (show $ getDirInfo $ getRootDirectory fs)

removeFileOrDirectory :: FilePath -> ExceptT FSException (State FSState) ()
removeFileOrDirectory path = do
  if (isAbsolute path) then
    removeInner path
  else do
    FSState{curDirectoryPath = curPath} <- get
    removeInner $ curPath </> path
  where
    removeInner realPath = do
      normPath <- getNormalisedPath realPath
      let (pathToContDir, name) = splitFileName $ normPath
      if (name == []) then
        throwE $ UnsupportedOperationArgument path
      else do
        dir <- getDirectoryByPath pathToContDir
        let newContent = Map.delete name (getDirContents dir)
        updateFileSystem pathToContDir dir{getDirContents = newContent}
        updatePathForRootDirectory

createDirectory :: String -> ExceptT FSException (State FSState) ()
createDirectory name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newDir = defaultNewDirectory name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Right newDir) (getDirContents curDir)
    updateFileSystem relDirPath curDir{getDirContents = newDirContents}

createFile :: String -> ExceptT FSException (State FSState) ()
createFile name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newFile = defaultNewFile name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Left newFile) (getDirContents curDir)
    updateFileSystem relDirPath curDir{getDirContents = newDirContents}

findFile :: String -> ExceptT FSException (State FSState) String
findFile fileName = do
  curDir <- getCurFSDirectory
  let files = searchForFileInDirectory fileName curDir
  if (files == []) then
    throwE FileNotFound
  else
    return $ intercalate "\n" files
  where
    searchForFileInDirectory :: String -> Directory -> [FilePath]
    searchForFileInDirectory name dir = do
      let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents dir
      let filesInDir =
            map (\x -> getFilePath $ getFileInfo x)
              (filter (\x -> (getFileName x) == name) $ lefts dirElements)
      let subDirs = rights $ dirElements
      let filesInSubDir = concat $ map (searchForFileInDirectory name) subDirs
      filesInDir ++ filesInSubDir

writeToFile :: B.ByteString -> FilePath -> ExceptT FSException (State FSState) ()
writeToFile content path = do
  if (isAbsolute path) then do
    writeToFileInner path
  else do
    FSState{curDirectoryPath = curPath} <- get
    writeToFileInner $ curPath </> path
  where
    writeToFileInner :: FilePath -> ExceptT FSException (State FSState) ()
    writeToFileInner realPath = do
      let (pathToContDir, name) = splitFileName realPath
      dir <- getDirectoryByPath pathToContDir
      fileOrDir <- lookupInDirectory dir name
      case (fileOrDir) of
        (Right _  ) -> throwE NotFile
        (Left file) -> do
          let modifiedFile = file{getFileData =  content}
          let newDirContents = Map.insert name (Left modifiedFile) (getDirContents dir)
          updateFileSystem pathToContDir dir{getDirContents = newDirContents}

goToDirectory :: FilePath -> ExceptT FSException (State FSState) ()
goToDirectory path = do
  FSState{fileSystem = fs, curDirectoryPath = curPath} <- get
  if (isAbsolute path) then do
    normSplittedPath <- getNormalisedSplittedPath path
    let absFSPath = joinPath normSplittedPath
    goToDirectoryInner absFSPath (getRootDirectory fs) normSplittedPath
  else do
    normSplittedPath <- getNormalisedSplittedPath (curPath </> path)
    let absFSPath = joinPath normSplittedPath
    goToDirectoryInner absFSPath (getRootDirectory fs) normSplittedPath
  where
    goToDirectoryInner :: FilePath -> Directory -> [FilePath]
                       -> ExceptT FSException (State FSState) ()
    goToDirectoryInner newPath _ [] = do
      st@FSState{} <- get
      put st {curDirectoryPath = newPath}
    goToDirectoryInner newPath dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> goToDirectoryInner newPath dir' xs

fileContent :: FilePath -> ExceptT FSException (State FSState) B.ByteString
fileContent path = runImmutableFunction getFileContent path
  where
    getFileContent :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) B.ByteString
    getFileContent _   []     = throwE NoSuchFileOrDirectory
    getFileContent dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left file) ->
          if (xs == []) then return $ getFileData file
          else throwE NotDirectory
        (Right dir') ->
          if (xs == []) then throwE NoSuchFileOrDirectory
          else getFileContent dir' xs

information :: FilePath -> ExceptT FSException (State FSState) String
information path = runImmutableFunction getInformation path
  where
    getInformation :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) String
    getInformation dir []     = return $ "ROOT Directory\n" ++
                                            (show $ getDirInfo dir)
    getInformation dir (x:xs) = do
      a <- lookupInDirectory dir x
      case a of
        (Left file)  -> do
          if (xs == []) then
            return $ intercalate "\n" $ [getFileName, show . getFileInfo] <*> [file]
          else
            throwE NotDirectory
        (Right dir') -> do
          if (xs == []) then
              return $ intercalate "\n" $ [getDirName, show . getDirInfo] <*> [dir']
          else
            getInformation dir' xs

directoryContent :: FilePath -> ExceptT FSException (State FSState) String
directoryContent path = runImmutableFunction showContentInDirectory path
  where
    showContentInDirectory :: Directory -> [FilePath]
                           -> ExceptT FSException (State FSState) String
    showContentInDirectory dir [] =
      return $ intercalate ("\n") $ map fst $ Map.toList $ getDirContents dir
    showContentInDirectory dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> showContentInDirectory dir' xs

lookupInDirectory :: Directory -> String -> ExceptT FSException (State FSState) DirElement
lookupInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptT FSException (State FSState) Directory
getCurFSDirectory = do
  FSState{curDirectoryPath = path} <- get
  getDirectoryByPath path

getDirectoryByPath :: FilePath -> ExceptT FSException (State FSState) Directory
getDirectoryByPath path = do
  FSState{fileSystem = fs} <- get
  normSplittedPath <- getNormalisedSplittedPath path
  getDir (getRootDirectory fs) normSplittedPath
  where
    getDir :: Directory -> [FilePath] -> ExceptT FSException (State FSState) Directory
    getDir dir [] = return dir
    getDir dir (x : xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NoSuchFileOrDirectory
        (Right dir') -> getDir dir' xs

updatePathForRootDirectory :: ExceptT FSException (State FSState) ()
updatePathForRootDirectory = do
  st@FSState{fileSystem = fs, curDirectoryPath = path} <- get
  normSplittedPath <- getNormalisedSplittedPath path
  let newCurDirectory = helper "" normSplittedPath (getRootDirectory fs)
  put st{curDirectoryPath = newCurDirectory}
  where
    helper :: FilePath -> [FilePath] -> Directory -> FilePath
    helper acc [] _ = acc
    helper acc (x : xs) dir = do
      case (Map.lookup x (getDirContents dir)) of
        Nothing  -> acc
        (Just a) ->
          case a of
            (Left  _   ) -> acc
            (Right dir') -> helper (acc </> x) xs dir'

updateFileSystem :: FilePath -> Directory -> ExceptT FSException (State FSState) ()
updateFileSystem path newDir = do
  st@FSState{fileSystem = fs} <- get
  let rootDir = getRootDirectory fs
  normSplittedPath <- getNormalisedSplittedPath path
  newRootDir <- updateDir rootDir newDir normSplittedPath
  let newFs = fs{getRootDirectory = newRootDir}
  put st{fileSystem = newFs}
  where
    updateDir :: Directory -> Directory -> [FilePath]
              ->  ExceptT FSException (State FSState) Directory
    updateDir _   nDir []     = return nDir
    updateDir dir nDir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE FSInconsistent
        (Right dir') -> do
          let dirContents = getDirContents dir
          updatedDir <- updateDir dir' nDir xs
          let newDirContents = Map.insert x (Right updatedDir) dirContents
          return dir{getDirContents = newDirContents}

runImmutableFunction :: forall a. (Directory -> [FilePath] -> (ExceptState a))
                     -> FilePath -> (ExceptState a)
runImmutableFunction foo path = do
  FSState{fileSystem = fs, curDirectoryPath = curPath} <- get
  if (isAbsolute path) then do
    normSplittedPath <- getNormalisedSplittedPath path
    foo (getRootDirectory fs) normSplittedPath
  else do
    normSplittedPath <- getNormalisedSplittedPath (curPath </> path)
    foo (getRootDirectory fs) normSplittedPath
