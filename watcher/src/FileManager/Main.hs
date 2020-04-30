module FileManager.Main where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Either (isLeft, isRight, lefts, rights)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import FileManager.Loader (getFileSystem)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (isAbsolute, splitDirectories, (</>))

data FSState = FSState
                 { fileSystem   :: FileSystem
                 , curDirectory :: FilePath
                 } deriving (Show)

data FSException
  = NoSuchFileOrDirectory
  | NotDirectory
  | NotFile
  | FileNotFound
  | PermissionDenied
  deriving (Show)

---TODO solve ByteString vs String problem
main :: IO ()
main = do
  fs <- getCurrentDirectory >>= \curDir -> getFileSystem curDir
  let state = FSState fs ""
  s <- getLine
  let (res2, st2) = runState (runExceptT $ goToDirectory s) state
  case res2 of
    Left err      -> putStrLn $ show (err :: FSException)
    Right _       -> putStrLn "success"

  fileToFind <- getLine
  let (res, st) = runState (runExceptT $ findFile fileToFind) st2
  case res of
    Left err      -> putStrLn $ show (err :: FSException)
    Right content -> putStrLn $ content

  -- let (res3, st3) = runState (runExceptT $ directoryContent "") st2
  -- case res3 of
  --   Left err      -> putStrLn $ show (err :: FSException)
  --   Right content -> putStrLn $ content
  -- fileName <- getLine
  -- let (res4, st4) = runState (runExceptT $ fileContent fileName) st3
  -- case res4 of
  --   Left err      -> putStrLn $ show (err :: FSException)
  --   Right content -> B.putStrLn $ content

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
    searchForFileInDirectory fileName dir = do
      let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents dir
      let filesInDir =
            map (\x -> getFilePath $ getFileInfo x)
              (filter (\x -> (getFileName x) == fileName) $ lefts dirElements)
      let subDirs = rights $ dirElements
      let filesInSubDir = concat $ map (searchForFileInDirectory fileName) subDirs
      filesInDir ++ filesInSubDir

-- TODO : "../.." и тому подобное
goToDirectory :: FilePath -> ExceptT FSException (State FSState) ()
goToDirectory pathToDir = do
  FSState{fileSystem = fs} <- get
  if (isAbsolute pathToDir) then do
    let splittedPath = tail $ splitDirectories $ pathToDir
    let simplePath = intercalate "/" splittedPath
    goToDirectoryInner simplePath (getRootDirectory fs) splittedPath
  else do
    FSState{fileSystem = fs, curDirectory = pathToCurDir} <- get
    let splittedPath = splitDirectories $ pathToDir
    let simplePath = pathToCurDir </> (intercalate "/" splittedPath)
    goToDirectoryInner simplePath (getRootDirectory fs) splittedPath
  where
    goToDirectoryInner :: FilePath -> Directory -> [FilePath]
                       -> ExceptT FSException (State FSState) ()
    goToDirectoryInner path dir [] = do
      st@FSState{fileSystem = fs, curDirectory = pathDir} <- get
      put st {curDirectory = path}
      return ()
    goToDirectoryInner path dir (x:xs) = do
      fileOrDir <- isInDirectory dir x
      case fileOrDir of
        (Left file)  -> throwE NotDirectory
        (Right dir') -> goToDirectoryInner path dir' xs

fileContent :: FilePath -> ExceptT FSException (State FSState) B.ByteString
fileContent path = runFunctionWithPath getFileContent path
  where
    getFileContent :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) B.ByteString
    getFileContent dir []     = throwE NoSuchFileOrDirectory
    getFileContent dir (x:xs) = do
      fileOrDir <- isInDirectory dir x
      case fileOrDir of
        (Left file) ->
          if (xs == []) then return $ getFileData file
          else throwE NotDirectory
        (Right dir') ->
          if (xs == []) then throwE NoSuchFileOrDirectory
          else getFileContent dir' xs

information :: FilePath -> ExceptT FSException (State FSState) String
information path = runFunctionWithPath getInformation path
  where
    getInformation :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) String
    getInformation dir []     = return $ show $ getDirInfo dir
    getInformation dir (x:xs) = do
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
directoryContent path = runFunctionWithPath showContentInDirectory path
  where
    showContentInDirectory :: Directory -> [FilePath]
                           -> ExceptT FSException (State FSState) String
    showContentInDirectory dir [] =
      return $ intercalate ("\n") $ map fst $ Map.toList $ getDirContents dir
    showContentInDirectory dir (x:xs) = do
      fileOrDir <- isInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> showContentInDirectory dir' xs

isInDirectory :: Directory -> FilePath -> ExceptT FSException (State FSState) DirElement
isInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptT FSException (State FSState) Directory
getCurFSDirectory = do
  FSState{fileSystem = fs, curDirectory = pathDir} <- get
  getDir (getRootDirectory fs) (splitDirectories pathDir)
  where
    getDir :: Directory -> [FilePath] -> ExceptT FSException (State FSState) Directory
    getDir dir [] = return dir
    getDir dir (x : xs) = do
      fileOrPath <- isInDirectory dir x
      case fileOrPath of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> getDir dir' xs
type ExceptState a = ExceptT FSException (State FSState) a

runFunctionWithPath :: (Directory -> [FilePath] -> (ExceptState a)) -> FilePath
                    -> (ExceptState a)
runFunctionWithPath foo path = do
  if (isAbsolute path) then do
    FSState{fileSystem = fs} <- get
    let dirs = tail $ splitDirectories path
    foo (getRootDirectory fs) dirs
  else do
    FSState{fileSystem = fs, curDirectory = pathToCurDir} <- get
    let absPath = pathToCurDir </> path
    let dirs = splitDirectories absPath
    foo (getRootDirectory fs) dirs
