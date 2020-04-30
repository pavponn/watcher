module FileManager.Loader
  ( getFileSystem
  ) where

import Control.Monad
import qualified Data.ByteString as B
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getFileSize,
                         getModificationTime, getPermissions, listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix (splitFileName, (</>))

getFileSystem :: FilePath -> IO FileSystem
getFileSystem dirPath = do
   let (path, name) = splitFileName dirPath
   directory <- visitDirectory path name
   return $ FileSystem directory path

visitDirectory :: FilePath -> FilePath -> IO Directory
visitDirectory path name = do
  let actualPath = path </> name
  permissions <- getPermissions actualPath
  modificationTime <- getModificationTime actualPath
  dirSize <- getFileSize actualPath
  let dirInfo = DirInfo
                  { getDirSize = dirSize
                  , getDirPath = actualPath
                  , getDirPermissions = permissions
                  , getDirModificationTime = modificationTime
                  }
  list <- listDirectory actualPath
  filesAndDirs <- filterM (\x -> not <$> pathIsSymbolicLink (actualPath </> x)) list
  dirs  <- filterM (\x -> doesDirectoryExist (actualPath </> x)) filesAndDirs
  files <- filterM (\x -> doesFileExist (actualPath </> x)) filesAndDirs
  contentDirectories <- zip dirs <$> (map Right) <$> mapM (visitDirectory actualPath) dirs
  contentFiles <- zip files <$> (map Left) <$> mapM (visitFile actualPath) files
  return $ Directory
             { getDirName = name
             , getDirInfo = dirInfo
             , getDirContents = Map.fromList $ contentDirectories ++ contentFiles
             }

visitFile :: FilePath -> FilePath -> IO File
visitFile path name = do
  let actualPath = (path </> name)
  permissions <- getPermissions actualPath
  modificationTime <- getModificationTime actualPath
  fileSize <- getFileSize actualPath
  fileData <- B.readFile actualPath
  let fileInfo = FileInfo
                   { getFileType = "X3 4TO ETO"
                   , getFilePath = actualPath
                   , getFileSizeBytes = fileSize
                   , getFilePermissions = permissions
                   , getFileModificationTime = modificationTime
                   }
  return $ File
             { getFileName = name
             , getFileInfo = fileInfo
             , getFileData = fileData
             }
