 module Loaders.Uploader
  ( uploadFileSystem
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive,
                         removeFile, writable)
import System.FilePath.Posix (takeDirectory, (</>))
import Utils.LoaderUtils (isDirectory, isFile, listExceptionHandler)

uploadFileSystem :: FileSystem -> IO ()
uploadFileSystem fs = do
  uploadDirectory $ getRootDirectory fs

uploadDirectory :: Directory -> IO ()
uploadDirectory dir = do
  let path = getDirPath $ getDirInfo dir
  (createDirectoryIfMissing True path) `catch` itakSoidetHandler
  let dirContents = getDirContents dir
  list <- listDirectory path `catch` listExceptionHandler
  dirNames <- filterM (\x -> isDirectory path x) list
  fileNames <- filterM (\x -> isFile path x) list
  let dirsToBeRemoved =  map (path </>) $ filter (\x -> not $ Map.member x dirContents) dirNames
  let filesToBeRemoved = map (path </>) $ filter (\x -> not $ Map.member x dirContents) fileNames
  forM_ dirsToBeRemoved removeDirectorySafe
  forM_ filesToBeRemoved removeFileSafe
  let (filesInDir, subDirs) = (partitionEithers . map snd) $ Map.toList dirContents
  forM_ filesInDir uploadFile
  forM_ subDirs uploadDirectory
  -- upload VCS

uploadFile :: File -> IO ()
uploadFile file = do
  let filePath = getFilePath $ getFileInfo file
  let content = getFileData file
  createDirectoryIfMissing True $ takeDirectory filePath
  if (not $ writable $ getFilePermissions $ getFileInfo file) then
    return ()
  else
    B.writeFile filePath content `catch` nezapisalos file

removeDirectorySafe :: FilePath -> IO ()
removeDirectorySafe path = do
  removeDirectoryRecursive path `catch` (removeDirExceptionHandler path)

removeFileSafe :: FilePath -> IO ()
removeFileSafe path = do
  removeFile path `catch` (removeFileExceptionHandler path)

nezapisalos :: File -> SomeException -> IO ()
nezapisalos file = \_ -> putStrLn $ "ne zapisal :/\n" ++ (show file)

itakSoidetHandler :: SomeException -> IO ()
itakSoidetHandler = \_ -> putStrLn "ne sozdal :/"

removeDirExceptionHandler :: FilePath -> SomeException -> IO ()
removeDirExceptionHandler path = \_ -> putStrLn $ "Unable to remove directory " ++ path

removeFileExceptionHandler :: FilePath -> SomeException -> IO ()
removeFileExceptionHandler path = \_ -> putStrLn $ "Unable to remove file " ++ path
