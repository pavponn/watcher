{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.FileManagerHandlers
  ( createDirectory
  , createFile
  , findFile
  , goToDirectory
  , fileContent
  , information
  , directoryContent
  , writeToFile
  , removeFileOrDirectory
  ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime (..))
import FileSystemTypes
import System.FilePath ((</>))
import System.FilePath.Posix (isAbsolute, joinPath, splitFileName)
import Utils.FilePathUtils
import Utils.FileSystemUtils

-- | Accepts path to file or directory and removes it from file system,
-- changing current state: file system and all current paths.
-- Throws `NotValidPath` if path is invalid, throws `UnsupportedOperation`,
-- if path is a path to root directory.
removeFileOrDirectory :: FilePath -> ExceptState ()
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
        throwE $ UnsupportedOperation $ "Can't remove " ++ path
      else do
        dir <- getDirectoryByPath pathToContDir
        let newContent = Map.delete name (getDirContents dir)
        updateFileSystem pathToContDir dir{getDirContents = newContent}
        updateSpecialPaths

-- | Creates new directory in current directory with specified name.
-- Updates current state (file system). Throws `DuplicateFileOrDirectory`if
-- file/directory with providied name already exists.
createDirectory :: String -> ExceptState ()
createDirectory name = do
  checkIfNameIsValid name
  FSState{curFileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  checkDirWritablePermissions curDir
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newDir = defaultNewDirectory name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Right newDir) (getDirContents curDir)
    updateFileSystem relDirPath curDir{getDirContents = newDirContents}

-- | Creates new file in current directory with specified name.
-- Updates current state (file system). Throws `DuplicateFileOrDirectory`if
-- file/directory with providied name already exists.
createFile :: (String, UTCTime) -> ExceptState ()
createFile (name, time) = do
  checkIfNameIsValid name
  FSState{curFileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  checkDirWritablePermissions curDir
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newFile = defaultNewFile name ((getPathToRootDirectory fs) </> relDirPath) time
    let newDirContents = Map.insert name (Left newFile) (getDirContents curDir)
    updateFileSystem relDirPath curDir{getDirContents = newDirContents}

-- | Returns a string that contains all paths to files with specified name.
findFile :: String -> ExceptState String
findFile fileName = do
  curDir <- getCurFSDirectory
  let files = searchForFileInDirectory curDir
  if (files == []) then
    throwE FileNotFound
  else
    return $ intercalate "\n" files
  where
    searchForFileInDirectory :: Directory -> [FilePath]
    searchForFileInDirectory dir = do
      let dirElements = map snd $ Map.toList $ getDirContents dir
      let filesInDir =
            map (\x -> getFilePath $ getFileInfo x)
              (filter filePredicate $ lefts dirElements)
      let subDirs = rights dirElements
      let filesInSubDir = concat $ map searchForFileInDirectory subDirs
      filesInDir ++ filesInSubDir
    filePredicate :: File -> Bool
    filePredicate = \x -> (getFileName x) == fileName

-- | Accepts new file's content (as a `ByteString`) and path to file, changes
-- this file's content. Updates state (file system). Throws `NotFile` if
-- provided path is not a path to file. Throws `NoSuchFileOrDirectory`
-- if there is no such file, `NotValidPath` if path is invalid.
writeToFile :: (B.ByteString, FilePath, UTCTime) -> ExceptState ()
writeToFile (content, path, curTime) = do
  if (isAbsolute path) then do
    writeToFileInner path
  else do
    FSState{curDirectoryPath = curPath} <- get
    writeToFileInner $ curPath </> path
  where
    writeToFileInner :: FilePath -> ExceptState ()
    writeToFileInner realPath = do
      let (pathToContDir, name) = splitFileName realPath
      dir <- getDirectoryByPath pathToContDir
      fileOrDir <- lookupInDirectory dir name
      case (fileOrDir) of
        (Right _  ) -> throwE NotFile
        (Left file) -> do
          checkFileWritablePermissions file
          let fileInfo = getFileInfo file
          let newFileInfo = fileInfo
                              { getFileSizeBytes = (toInteger . B.length) content
                              , getFileModificationTime = curTime
                              }
          let modifiedFile = file{getFileData = content, getFileInfo = newFileInfo}
          let newDirContents = Map.insert name (Left modifiedFile) (getDirContents dir)
          updateFileSystem pathToContDir dir{getDirContents = newDirContents}

-- | Accepts path to directory to go to. Updates state
-- (file system, all special paths) Throws `NoSuchFileOrDirectory`
-- if there is no such directory, `NotValidPath` if path is invalid.
goToDirectory :: FilePath -> ExceptState ()
goToDirectory path = do
  FSState{curFileSystem = fs, curDirectoryPath = curPath} <- get
  if (isAbsolute path) then do
    normSplittedPath <- getNormalisedSplittedPath path
    let absFSPath = joinPath normSplittedPath
    goToDirectoryInner absFSPath Nothing (getRootDirectory fs) normSplittedPath
  else do
    normSplittedPath <- getNormalisedSplittedPath (curPath </> path)
    let absFSPath = joinPath normSplittedPath
    goToDirectoryInner absFSPath Nothing (getRootDirectory fs) normSplittedPath
  where
    goToDirectoryInner :: FilePath -> Maybe FilePath -> Directory -> [FilePath]
                       -> ExceptState ()
    goToDirectoryInner newPath maybeVCSPath dir [] = do
      st@FSState{} <- get
      case getVCSStorage dir of
        Nothing  -> put st{curDirectoryPath = newPath, curVCSPath = maybeVCSPath}
        (Just _) -> do
          newVCSPath <- getFSPathForDirectory dir
          put st{curDirectoryPath = newPath, curVCSPath = (Just newVCSPath)}
    goToDirectoryInner newPath maybeVCSPath dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> do
          case getVCSStorage dir of
            Nothing  -> goToDirectoryInner newPath maybeVCSPath dir' xs
            (Just _) -> do
              newVCSPath <- getFSPathForDirectory dir
              goToDirectoryInner newPath (Just newVCSPath) dir' xs

-- | Returns content of file that coresponds to provided file path.
-- Throws `NoSuchFileOrDirectory` if there is no such file,
-- `NotValidPath` if path is invalid.
fileContent :: FilePath -> ExceptState B.ByteString
fileContent path = runImmutableFunction getFileContent path
  where
    getFileContent :: Directory -> [FilePath] -> ExceptState B.ByteString
    getFileContent _   []     = throwE NoSuchFileOrDirectory
    getFileContent dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left file) ->
          if (xs == []) then do
            checkFileReadablePermissions file
            return $ getFileData file
          else throwE NoSuchFileOrDirectory
        (Right dir') ->
          if (xs == []) then throwE NoSuchFileOrDirectory
          else getFileContent dir' xs

-- | Accepts path to file/directory and returns information about it as a String.
-- Throws `NoSuchFileOrDirectory` if there is no such file/directory, `NotValidPath`
-- if path is invalid.
information :: FilePath -> ExceptState String
information path = runImmutableFunction getInformation path
  where
    getInformation :: Directory -> [FilePath] -> ExceptState String
    getInformation dir [] = do
      let filesInDirectory = getAllFilesInDirRecursive dir
      return $ "ROOT Directory\n" ++ (show $ getDirInfo dir) ++
        "\nTotal files: " ++ (show $ length filesInDirectory)
    getInformation dir (x:xs) = do
      a <- lookupInDirectory dir x
      case a of
        (Left file)  -> do
          if (xs == []) then
            return $ intercalate "\n" $ [getFileName, show . getFileInfo] <*> [file]
          else
            throwE NoSuchFileOrDirectory
        (Right dir') -> do
          if (xs == []) then do
              let filesInDirectory = getAllFilesInDirRecursive dir'
              return $ intercalate "\n" $
                  ([getDirName, show . getDirInfo] <*> [dir']) ++
                    ["Total files: " ++ (show $ length filesInDirectory)]
          else
            getInformation dir' xs

-- | Accepts path to directory and returns its content as a String.
-- Throws `NoSuchFileOrDirectory` if there is no such file/directory, `NotValidPath`
-- if path is invalid.
directoryContent :: FilePath -> ExceptState String
directoryContent path = runImmutableFunction showContentInDirectory path
  where
    showContentInDirectory :: Directory -> [FilePath] -> ExceptState String
    showContentInDirectory dir [] =
      return $ intercalate ("\n") $ map fst $ Map.toList $ getDirContents dir
    showContentInDirectory dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NoSuchFileOrDirectory
        (Right dir') -> showContentInDirectory dir' xs

runImmutableFunction :: forall a. (Directory -> [FilePath] -> (ExceptState a))
                     -> FilePath -> (ExceptState a)
runImmutableFunction foo path = do
  FSState{curFileSystem = fs, curDirectoryPath = curPath} <- get
  if (isAbsolute path) then do
    normSplittedPath <- getNormalisedSplittedPath path
    foo (getRootDirectory fs) normSplittedPath
  else do
    normSplittedPath <- getNormalisedSplittedPath (curPath </> path)
    foo (getRootDirectory fs) normSplittedPath
