module FileManager.FileSystemUtils
  ( lookupInDirectory
  , getCurFSDirectory
  , getDirElementByPath
  , getFileByPath
  , getDirectoryByPath
  , updateSpecialPaths
  , updateFileSystem
  , retractVCSStorage
  , getVCSPath
  , getAllFilesInDirAndSubDirs
  ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Either (lefts, rights)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import FileManager.FilePathUtils
import FileManager.FileSystemTypes
import System.FilePath ((</>))

lookupInDirectory :: Directory -> String -> ExceptState DirElement
lookupInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptState Directory
getCurFSDirectory = do
  FSState{curDirectoryPath = path} <- get
  getDirectoryByPath path `catchE` (\_ -> throwE FSInconsistent)

getDirElementByPath :: FilePath -> ExceptState DirElement
getDirElementByPath path = do
  FSState{curFileSystem = fs} <- get
  normSplittedPath <- getNormalisedSplittedPath path
  getDirElement (getRootDirectory fs) normSplittedPath
  where
    getDirElement :: Directory -> [FilePath] -> ExceptState DirElement
    getDirElement dir [] = return $ Right dir
    getDirElement dir (x : xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left file)  ->
          if (xs == []) then return $ Left file
          else throwE NoSuchFileOrDirectory
        (Right dir') -> getDirElement dir' xs

getFileByPath :: FilePath -> ExceptState File
getFileByPath path = do
  fileOrDir <- getDirElementByPath path
  case fileOrDir of
    (Left  file) -> return file
    (Right _   ) -> throwE NoSuchFileOrDirectory

getDirectoryByPath :: FilePath -> ExceptState Directory
getDirectoryByPath path = do
  fileOrDir <- getDirElementByPath path
  case fileOrDir of
    (Left  _  ) -> throwE NoSuchFileOrDirectory
    (Right dir) -> return dir

updateSpecialPaths :: ExceptT FSException (State FSState) ()
updateSpecialPaths = do
  st@FSState{curFileSystem = fs, curDirectoryPath = path} <- get
  normSplittedPath <- getNormalisedSplittedPath path
  let rootDir = getRootDirectory fs
  let maybeStart = if (isNothing $ getVCSStorage rootDir) then Nothing else Just ""
  let (newCurDirPath, newVCSPath)  = helper "" maybeStart normSplittedPath rootDir
  put st{curDirectoryPath = newCurDirPath, curVCSPath = newVCSPath}
  where
    helper :: FilePath -> Maybe FilePath -> [FilePath] -> Directory -> (FilePath, Maybe FilePath)
    helper acc maybePath []       _   = (acc, maybePath)
    helper acc maybePath (x : xs) dir = do
      case (Map.lookup x (getDirContents dir)) of
        Nothing -> (acc, maybePath)
        (Just a) ->
          case a of
            (Left _    ) -> (acc, maybePath)
            (Right dir') -> do
              let newAcc = acc </> x
              let maybePath' = if (isNothing $ getVCSStorage dir') then maybePath else Just newAcc
              helper newAcc maybePath' xs dir'

updateFileSystem :: FilePath -> Directory -> ExceptState ()
updateFileSystem path newDir = do
  st@FSState{curFileSystem = fs} <- get
  let rootDir = getRootDirectory fs
  normSplittedPath <- getNormalisedSplittedPath path
  newRootDir <- updateDir rootDir newDir normSplittedPath
  let newFs = fs{getRootDirectory = newRootDir}
  put st{curFileSystem = newFs}
  where
    updateDir :: Directory -> Directory -> [FilePath] -> ExceptState Directory
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

retractVCSStorage :: Directory -> ExceptState VCSStorage
retractVCSStorage dir = do
  let maybeStorage = getVCSStorage dir
  case maybeStorage of
    Nothing  -> throwE $ VCSException "VCS isn't initialized"
    (Just s) -> return s

getVCSPath :: ExceptState FilePath
getVCSPath = do
  FSState{curVCSPath = maybePath} <- get
  case maybePath of
    Nothing  -> throwE $ ImpossibleToPerform "Current directory is not a part of VCS"
    (Just p) -> return p

getAllFilesInDirAndSubDirs :: Directory -> ExceptState [File]
getAllFilesInDirAndSubDirs curDir = do
  let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents curDir
  filesInSubDir <- mapM getAllFilesInDirAndSubDirs (rights dirElements)
  return $ (lefts dirElements) ++ (concat filesInSubDir)
