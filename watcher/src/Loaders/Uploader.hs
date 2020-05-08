 module Loaders.Uploader
  ( uploadFileSystem
  ) where

import Control.Exception (SomeException, catch, throw)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive,
                         removeFile, writable)
import System.FilePath.Posix (takeDirectory, (</>))
import Utils.LoaderUtils
import System.FilePath.Posix (dropTrailingPathSeparator)

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
  uploadVCS path (getVCSStorage dir)

uploadFile :: File -> IO ()
uploadFile file = do
  let filePath = getFilePath $ getFileInfo file
  let content = getFileData file
  createDirectoryIfMissing True $ takeDirectory filePath
  if (not $ writable $ getFilePermissions $ getFileInfo file) then
    return ()
  else
    B.writeFile filePath content `catch` nezapisalos file

uploadVCS :: FilePath -> Maybe VCSStorage -> IO ()
uploadVCS _    Nothing   = return ()
uploadVCS path (Just st) = do
  let vcsPath = path </> ".vcs"
  let vcsFilesData = Map.toList $ getVCSFiles st
  forM_ vcsFilesData (uploadVCSFile vcsPath) `catch` (vcsExceptionHandler vcsPath 1)
  isDir <- isDirectory path ".vcs"
  if isDir then
    (createIndexFile vcsPath st) `catch` (vcsExceptionHandler vcsPath 2)
  else
    return ()
  where
    vcsExceptionHandler :: FilePath -> Integer -> SomeException -> IO ()
    vcsExceptionHandler vcsPath i = \_ -> do
      putStrLn $ "error while writing VCS, VCS won't be uploaded to system " ++ show i
      removeDirectorySafe vcsPath

createIndexFile :: FilePath -> VCSStorage -> IO ()
createIndexFile vcsPath st = do
  let fileName = "index_" ++ (show $ getRevisionsNum st)
  let indexesToMessages = Map.toList $ Map.unions $ Map.elems $ getVCSFiles st
  let content = B.concat $ map getLineForRev indexesToMessages
  createDirectoryIfMissing True vcsPath
  B.writeFile (vcsPath </> fileName) content `catch` rethrowHandler
  where
    getLineForRev = \(index, (_, msg)) -> BU.fromString $ (show index) ++ "->" ++ msg ++ "\n"

uploadVCSFile :: FilePath -> (FilePath, (Map.Map Integer (File, String))) -> IO ()
uploadVCSFile vcsPath (pathToFile, revisions) = do
  let pathToFileDir = (dropTrailingPathSeparator vcsPath) ++ pathToFile ++ "/"
  let listOfRevs = Map.toList revisions
  forM_ listOfRevs (writeRev pathToFileDir) `catch` rethrowHandler
  where
    writeRev :: FilePath -> (Integer, (File, String)) -> IO ()
    writeRev pathToFileDir = \(index, (file, _)) ->
      writeToFileWithName pathToFileDir (show index) (getFileData file)

writeToFileWithName :: FilePath -> FilePath -> B.ByteString -> IO ()
writeToFileWithName dirPath fileName content = do
  let realPath = dirPath </> fileName
  createDirectoryIfMissing True $ takeDirectory dirPath
  B.writeFile realPath content `catch` rethrowHandler

removeDirectorySafe :: FilePath -> IO ()
removeDirectorySafe path = do
  removeDirectoryRecursive path `catch` (removeDirExceptionHandler path)

removeFileSafe :: FilePath -> IO ()
removeFileSafe path = do
  removeFile path `catch` (removeFileExceptionHandler path)

rethrowHandler :: SomeException -> IO ()
rethrowHandler  = \ex -> throw ex

nezapisalos :: File -> SomeException -> IO ()
nezapisalos file = \_ -> putStrLn $ "ne zapisal :/\n" ++ (show file)

itakSoidetHandler :: SomeException -> IO ()
itakSoidetHandler = \_ -> putStrLn "ne sozdal :/"

removeDirExceptionHandler :: FilePath -> SomeException -> IO ()
removeDirExceptionHandler path = \_ -> putStrLn $ "Unable to remove directory " ++ path

removeFileExceptionHandler :: FilePath -> SomeException -> IO ()
removeFileExceptionHandler path = \_ -> putStrLn $ "Unable to remove file " ++ path
