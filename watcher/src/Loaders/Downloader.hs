module Loaders.Downloader
  ( getFileSystem
  ) where

import Control.Exception (catch)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import FileManager.FileSystemTypes
import Network.Mime (fileNameExtensions)
import System.Directory (getFileSize, getModificationTime, getPermissions, listDirectory)
import System.FilePath.Posix (dropTrailingPathSeparator, splitFileName, (</>))
import System.IO.Error (ioError, userError)
import Utils.LoaderUtils (isDirectory, isFile, listExceptionHandler, permsExceptionHandler)

-- | Returns `FileSystem` with the root at provided filepath.
getFileSystem :: FilePath -> IO FileSystem
getFileSystem dirPath = do
  let (path, name) = splitFileName $ dropTrailingPathSeparator dirPath
  isDir <- isDirectory path name
  if isDir then do
    rootDir <- visitDirectory path name
    return $ FileSystem rootDir (path </> name)
  else ioError $ userError $ "Can't get file system by path: " ++ dirPath

visitDirectory :: FilePath -> FilePath -> IO Directory
visitDirectory path name = do
  let actualPath = path </> name
  permissions <- getPermissions actualPath `catch` permsExceptionHandler
  dirSize <- getFileSize actualPath
  let dirInfo = DirInfo dirSize actualPath permissions
  list <- listDirectory actualPath `catch` listExceptionHandler
  dirs  <- filterM (\x -> liftM2 (&&) (isDirectory actualPath x) (return $ x /= ".vcs")) list
  files <- filterM (\x -> isFile actualPath x) list
  contentDirectories <- zip dirs <$> (map Right) <$> mapM (visitDirectory actualPath) dirs
  contentFiles <- zip files <$> (map Left) <$> mapM (visitFile actualPath) files
  let dirContents = Map.fromList $ contentDirectories ++ contentFiles
  return $ Directory name dirInfo dirContents Nothing

visitFile :: FilePath -> FilePath -> IO File
visitFile path name = do
  let actualPath = (path </> name)
  perms <- getPermissions actualPath `catch` permsExceptionHandler
  time <- getModificationTime actualPath
  fileSize <- getFileSize actualPath
  fileData <- B.readFile actualPath
  let fileTypes = T.unpack <$> (fileNameExtensions . T.pack) name
  let fileInfo = FileInfo fileTypes actualPath fileSize perms time
  return $ File name fileInfo fileData
