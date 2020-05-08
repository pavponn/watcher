module Loaders.Downloader
  ( getFileSystem
  ) where

import Control.Exception (catch)
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import FileSystemTypes
import Network.Mime (fileNameExtensions)
import System.Directory (getFileSize, getModificationTime, getPermissions, listDirectory)
import System.FilePath.Posix (dropTrailingPathSeparator, splitFileName, (</>), makeRelative)
import System.IO.Error (ioError, userError)
import Utils.LoaderUtils
import Utils.FileSystemUtils(getAllFilesInDirRecursive)

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
  isD <- isDirectory actualPath ".vcs"
  if (isD) then do
    storage <- (getMaybeVCSStorage $ actualPath </> ".vcs") `catch` maybeExceptionHandler
    return $ Directory name dirInfo dirContents storage
  else
    return $ Directory name dirInfo dirContents Nothing

visitFile :: FilePath -> FilePath -> IO File
visitFile path name = do
  let actualPath = (path </> name)
  perms <- getPermissions actualPath `catch` permsExceptionHandler
  time <- getModificationTime actualPath `catch` timeExceptionHandler
  fileSize <- getFileSize actualPath
  fileData <- B.readFile actualPath
  let fileTypes = T.unpack <$> (fileNameExtensions . T.pack) name
  let fileInfo = FileInfo fileTypes actualPath fileSize perms time
  return $ File name fileInfo fileData

getMaybeVCSStorage :: FilePath -> IO (Maybe VCSStorage)
getMaybeVCSStorage vcsPath = do
  fs <- getFileSystem vcsPath `catch` rethrowHandler
  let files = getAllFilesInDirRecursive $ getRootDirectory fs
  let indFiles = filter (\x -> (vcsPath </> "index") == (getFilePath $ getFileInfo x)) files
  let otherFiles = filter (\x -> (vcsPath </> "index") /= (getFilePath $ getFileInfo x)) files
  let macOSSpecific = filter (\x -> (getFileName x) /= ".DS_Store") otherFiles
  case indFiles of
    []            -> ioError $ userError $ "No index at: " ++ vcsPath
    (indexFile:_) -> do
      let indMap = getIndexToMessageMap indexFile
      let vcsMap = createVCSMap indMap vcsPath (Map.empty) macOSSpecific
      let revNum = (maximum $ -1 : (Map.keys indMap)) + 1
      return $ Just VCSStorage{getVCSFiles=vcsMap, getRevisionsNum=revNum}

type VCSMap = (Map.Map FilePath (Map.Map Integer (File, String)))

type IndexMap = Map.Map Integer String

createVCSMap :: IndexMap -> FilePath -> VCSMap -> [File] -> VCSMap
createVCSMap _ _ vcsMap [] = vcsMap
createVCSMap indexMap vcsPath vcsMap (file:files) = do
  let (dirpath, version) = splitFileName (getFilePath $ getFileInfo file)
  let realPath =  "/" ++ (dropTrailingPathSeparator $ makeRelative vcsPath dirpath)
  let ind = readIntDefault version
  let msg = Map.findWithDefault ".vcs-folder damaged default msg" ind indexMap
  let newVCSMap = insertIntoVCSMap vcsMap realPath ind file msg
  createVCSMap indexMap vcsPath newVCSMap files

insertIntoVCSMap :: VCSMap -> FilePath -> Integer -> File -> String -> VCSMap
insertIntoVCSMap vcsMap path ind file msg = do
  let (_, name) = splitFileName path
  let newFileInfo = (getFileInfo file){getFilePath = path}
  let fileToInsert = file{getFileInfo = newFileInfo, getFileName = name}
  if (Map.member path vcsMap) then do
    let innerMap = vcsMap Map.! path
    let newInnerMap = Map.insert ind (fileToInsert, msg) innerMap
    let newVcsMap = Map.insert path newInnerMap vcsMap
    newVcsMap
  else do
    let newVcsMap = Map.insert path (Map.fromList [(ind, (fileToInsert, msg))]) vcsMap
    newVcsMap

getIndexToMessageMap :: File -> IndexMap
getIndexToMessageMap file = do
  let content = getFileData file
  let pairs = map (\(f, s) -> (readIntDefault $ BS.toString f, BS.toString s)) $
        map (myBreakByteString (BS.fromString "->")) $ filter (B.empty /=) $ BS.lines content
  Map.fromList pairs

myBreakByteString :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
myBreakByteString split str = do
  let (f, s) = B.breakSubstring split str
  if ((B.length s) > 1) then
    (f, B.tail $ B.tail s)
  else
    (f, s)

readIntSafe :: String -> Maybe Integer
readIntSafe = \s -> case reads s of
  [(x, "")] -> Just x
  _         -> Nothing

readIntDefault :: String -> Integer
readIntDefault = \s -> case readIntSafe s of
  (Just x) -> x
  Nothing  -> -1
