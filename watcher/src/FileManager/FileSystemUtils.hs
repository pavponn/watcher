module FileManager.FileSystemUtils where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as Map
import FileManager.FilePathUtils
import FileManager.FileSystemTypes
import System.FilePath ((</>))

lookupInDirectory :: Directory -> String -> ExceptT FSException (State FSState) DirElement
lookupInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptT FSException (State FSState) Directory
getCurFSDirectory = do
  FSState{curDirectoryPath = path} <- get
  getDirectoryByPath path `catchE` (\_ -> throwE FSInconsistent)

getDirElementByPath :: FilePath -> ExceptT FSException (State FSState) DirElement
getDirElementByPath path = do
  FSState{curFileSystem = fs} <- get
  normSplittedPath <- getNormalisedSplittedPath path
  getDirElement (getRootDirectory fs) normSplittedPath
  where
    getDirElement :: Directory -> [FilePath] -> ExceptT FSException (State FSState) DirElement
    getDirElement dir [] = return $ Right dir
    getDirElement dir (x : xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left file)  ->
          if (xs == []) then return $ Left file
          else throwE NoSuchFileOrDirectory
        (Right dir') -> getDirElement dir' xs

getFileByPath :: FilePath -> ExceptT FSException (State FSState) File
getFileByPath path = do
  fileOrDir <- getDirElementByPath path
  case fileOrDir of
    (Left  file) -> return file
    (Right _   ) -> throwE NoSuchFileOrDirectory

getDirectoryByPath :: FilePath -> ExceptT FSException (State FSState) Directory
getDirectoryByPath path = do
  fileOrDir <- getDirElementByPath path
  case fileOrDir of
    (Left  _  ) -> throwE NoSuchFileOrDirectory
    (Right dir) -> return dir


updatePathForRootDirectory :: ExceptT FSException (State FSState) ()
updatePathForRootDirectory = do
  st@FSState{curFileSystem = fs, curDirectoryPath = path} <- get
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
  st@FSState{curFileSystem = fs} <- get
  let rootDir = getRootDirectory fs
  normSplittedPath <- getNormalisedSplittedPath path
  newRootDir <- updateDir rootDir newDir normSplittedPath
  let newFs = fs{getRootDirectory = newRootDir}
  put st{curFileSystem = newFs}
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

retractVCSStorage :: Directory -> ExceptT FSException (State FSState) VCSStorage
retractVCSStorage dir = do
  let maybeStorage = getVCSStorage dir
  case maybeStorage of
    Nothing  -> throwE $ VCSNotInitialised
    (Just s) -> return s

getVCSPath :: ExceptT FSException (State FSState) FilePath
getVCSPath = do
  FSState{curVCSPath = maybePath} <- get
  case maybePath of
    Nothing  -> throwE $ UnsupportedOperation "Current directory is not a part of VCS"
    (Just p) -> return p
