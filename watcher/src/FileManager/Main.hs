module FileManager.Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import FileSystemTypes
import Loader (getFileSystem)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getFileSize,
                         getModificationTime, getPermissions, listDirectory, pathIsSymbolicLink)
import System.FilePath.Posix (isAbsolute, splitDirectories)

data FSState = MyState
                 { fileSystem   :: FileSystem
                 , curDirectory :: Directory
                 } deriving (Show)

data FSException
  = NoSuchFileOrDirectory
  | NotDirectory
  | NotFile
  | PermissionDenied
  deriving (Show)

main :: IO ()
main = do
  fs <- getCurrentDirectory >>= \curDir -> getFileSystem curDir
  let state = MyState fs $ getRootDirectory fs
  s <- getLine
  let (res, st) = runState (runExceptT $ information s) state
  case res of
    Left err      -> putStrLn $ show (err :: FSException)
    Right content -> putStrLn content
  return ()

information :: FilePath -> ExceptT FSException (State FSState) String
information path = do
  if (isAbsolute path) then do
    MyState{fileSystem = fs} <- get
    let dirs = tail (splitDirectories path)
    getInformation (getRootDirectory fs) dirs
  else do
    MyState{curDirectory = dir} <- get
    let dirs = splitDirectories path
    getInformation dir dirs
  where
    getInformation :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) String
    getInformation dir []       = return $ show $ getDirInfo dir
    getInformation dir (x : xs) = do
      a <- isInDirectory dir x
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
directoryContent path = do
  if (isAbsolute path) then do
    MyState{fileSystem = fs} <- get
    let dirs = tail (splitDirectories path)
    showContentInDirectory (getRootDirectory fs) dirs
  else do
    MyState{curDirectory = dir} <- get
    showContentInDirectory dir (splitDirectories path)
  where
    showContentInDirectory :: Directory -> [FilePath]
                           -> ExceptT FSException (State FSState) String
    showContentInDirectory dir [] =
      return $ intercalate ("\n") $ map fst $ Map.toList $ getDirContents dir
    showContentInDirectory dir (x : xs) = do
      fileOrDir <- isInDirectory dir x
      case fileOrDir of
        (Left _)     -> throwE NotDirectory
        (Right dir') -> showContentInDirectory dir' xs

isInDirectory :: Directory -> FilePath -> ExceptT FSException (State FSState) DirElement
isInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a
