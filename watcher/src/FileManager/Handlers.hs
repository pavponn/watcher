{-# LANGUAGE ScopedTypeVariables #-}

module FileManager.Handlers where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import System.FilePath.Posix (dropTrailingPathSeparator, isAbsolute, normalise, splitDirectories,
                              (</>))
import System.Path.NameManip (guess_dotdot)

data FSState = FSState
  { fileSystem       :: FileSystem
  , curDirectoryPath :: FilePath
  } deriving (Show)

data FSException
  = NoSuchFileOrDirectory
  | NotDirectory
  | NotFile
  | FileNotFound
  | PermissionDenied
  | DuplicateFileOrDirectory String
  | FSInconsistent
  | NotValidPath String
  deriving (Show)

createDirectory :: String -> ExceptT FSException (State FSState) ()
createDirectory name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newDir = defaultNewDirectory name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Right newDir) (getDirContents curDir)
    updateFileSystem curDir{getDirContents = newDirContents}

createFile :: String -> ExceptT FSException (State FSState) ()
createFile name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newFile = defaultNewFile name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Left newFile) (getDirContents curDir)
    updateFileSystem curDir{getDirContents = newDirContents}

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

goToDirectory :: FilePath -> ExceptT FSException (State FSState) ()
goToDirectory path = do
  if (isAbsolute path) then do
    run path
  else do
    FSState{curDirectoryPath = curPath} <- get
    run $ curPath </> path
  where
    run :: FilePath -> ExceptT FSException (State FSState) ()
    run absPath = do
      FSState{fileSystem = fs} <- get
      let maybePath = customNormalise absPath
      case maybePath of
        Nothing   -> throwE $ NotValidPath path
        (Just np) -> do
          let splittedPath = getSplittedPath np
          let absoluteFSPath = intercalate "/" splittedPath
          goToDirectoryInner absoluteFSPath (getRootDirectory fs) splittedPath
    goToDirectoryInner :: FilePath -> Directory -> [FilePath] -> ExceptT FSException (State FSState) ()
    goToDirectoryInner newPath _ [] = do
      st@FSState{} <- get
      put st {curDirectoryPath = newPath}
      return ()
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
    getInformation dir []     = return $ show $ getDirInfo dir
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

lookupInDirectory :: Directory -> FilePath -> ExceptT FSException (State FSState) DirElement
lookupInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptT FSException (State FSState) Directory
getCurFSDirectory = do
  FSState{fileSystem = fs, curDirectoryPath = pathDir} <- get
  getDir (getRootDirectory fs) (splitDirectories pathDir)
  where
    getDir :: Directory -> [FilePath] -> ExceptT FSException (State FSState) Directory
    getDir dir [] = return dir
    getDir dir (x : xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE FSInconsistent
        (Right dir') -> getDir dir' xs

type ExceptState a = ExceptT FSException (State FSState) a

updateFileSystem :: Directory -> ExceptT FSException (State FSState) ()
updateFileSystem newDir = do
  st@FSState{fileSystem = fs, curDirectoryPath = path}<- get
  let rootDir = getRootDirectory fs
  let splittedPath = splitDirectories path
  newRootDir <- updateDir rootDir newDir splittedPath
  put st{fileSystem = fs{getRootDirectory = newRootDir}}
  return ()
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
          return dir'{getDirContents = newDirContents}

runImmutableFunction :: forall a. (Directory -> [FilePath] -> (ExceptState a))
                     -> FilePath -> (ExceptState a)
runImmutableFunction foo path = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then do
    let normalisedPath = customNormalise path
    run normalisedPath
  else do
    let normalisedPath = customNormalise $ curPath </> path
    run normalisedPath
  where
    run :: Maybe FilePath -> ExceptState a
    run maybePath = do
      FSState{fileSystem = fs} <- get
      case maybePath of
        (Just p) -> do
          let splittedPath = getSplittedPath p
          foo (getRootDirectory fs) splittedPath
        _           -> throwE $ NotValidPath path

customNormalise :: FilePath -> Maybe FilePath
customNormalise = removeDots . (dropTrailingPathSeparator . normalise)
  where
    removeDots path =
      case guess_dotdot path of
        Nothing  -> Nothing
        (Just a) ->
          case a of
            ('.' : chs) -> Just chs
            _           -> Just a

getSplittedPath :: FilePath -> [FilePath]
getSplittedPath path = do
  let splittedPath = splitDirectories $ dropTrailingPathSeparator path
  case splittedPath of
    ("/" : xs) -> xs
    _          -> splittedPath
