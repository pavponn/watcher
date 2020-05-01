module FileManager.Handlers where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import FileManager.Loader (getFileSystem)
import System.Directory (getCurrentDirectory)
import System.FilePath.Posix (isAbsolute, splitDirectories, dropTrailingPathSeparator, (</>))

data FSState = FSState
                 { fileSystem       :: FileSystem
                 , curDirectoryPath :: FilePath
                 } deriving (Show)

data FSException
  = NoSuchFileOrDirectory
  | NotDirectory
  | NotFile
  | FileNotFound
  | PermissionDenied
  | DuplicateFileOrDirectory String
  | FSInconsistent
  deriving (Show)

---TODO solve ByteString vs String problem
-- main :: IO ()
-- main = do
  -- fs <- getCurrentDirectory >>= \curDir -> getFileSystem curDir
  -- let st = FSState fs ""
  -- s0 <- getLine
  -- let (res0, st0) = runState (runExceptT $ goToDirectory s0) st
  -- case res0 of
  --   (Left err)   -> putStrLn $ show (err :: FSException)
  --   (Right cont) -> putStrLn "success"
  -- s <- getLine
  -- let (res, st1) = runState (runExceptT $ directoryContent s) st0
  -- case res of
  --   (Left err)   -> putStrLn $ show (err :: FSException)
  --   (Right cont) -> putStrLn cont
  -- s1 <- getLine
  -- let (res2, st2) = runState (runExceptT $ createDirectory s1) st1
  -- case res2 of
  --   (Left err) -> putStrLn $ show (err :: FSException)
  --   (Right _)  -> putStrLn "created"
  -- s2 <- getLine
  -- let (res3, st3) = runState (runExceptT $ goToDirectory s2) st2
  -- case res3 of
  --   (Left err)   -> putStrLn $ show (err :: FSException)
  --   (Right cont) -> putStrLn cont

createDirectory :: String -> ExceptT FSException (State FSState) ()
createDirectory name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newDir = defaultNewDirectory name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Right newDir) (getDirContents curDir)
    updateFileSystem curDir{getDirContents = newDirContents}

createFile :: String -> ExceptT FSException (State FSState) ()
createFile name = do
  FSState{fileSystem = fs, curDirectoryPath = relDirPath} <- get
  curDir <- getCurFSDirectory
  if (Map.member name (getDirContents curDir)) then
    throwE $ DuplicateFileOrDirectory name
  else do
    let newFile = defaultNewFile name ((getPathToRootDirectory fs) </> relDirPath)
    let newDirContents = Map.insert name (Left newFile) (getDirContents curDir)
    updateFileSystem curDir{getDirContents = newDirContents}

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
    searchForFileInDirectory name dir = do
      let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents dir
      let filesInDir =
            map (\x -> getFilePath $ getFileInfo x)
              (filter (\x -> (getFileName x) == name) $ lefts dirElements)
      let subDirs = rights $ dirElements
      let filesInSubDir = concat $ map (searchForFileInDirectory name) subDirs
      filesInDir ++ filesInSubDir

-- TODO : "../.." и тому подобное
goToDirectory :: FilePath -> ExceptT FSException (State FSState) ()
goToDirectory pathToDir = do
  FSState{fileSystem = fs, curDirectoryPath = pathToCurDir} <- get
  if (isAbsolute pathToDir) then do
    let splittedPath = tail $ getSplittedPath pathToDir
    let absoluteFSPath = intercalate "/" splittedPath
    goToDirectoryInner absoluteFSPath (getRootDirectory fs) splittedPath
  else do
    curDir <- getCurFSDirectory
    let splittedPath = getSplittedPath pathToDir
    let absoluteFSPath = pathToCurDir </> (intercalate "/" splittedPath)
    goToDirectoryInner absoluteFSPath curDir splittedPath
  where
    goToDirectoryInner :: FilePath -> Directory -> [FilePath]
                       -> ExceptT FSException (State FSState) ()
    goToDirectoryInner path _ [] = do
      st@FSState{} <- get
      put st {curDirectoryPath = path}
      return ()
    goToDirectoryInner path dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> goToDirectoryInner path dir' xs

fileContent :: FilePath -> ExceptT FSException (State FSState) B.ByteString
fileContent path = runImmutableFunction getFileContent path
  where
    getFileContent :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) B.ByteString
    getFileContent _   []     = throwE NoSuchFileOrDirectory
    getFileContent dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left file) ->
          if (xs == []) then return $ getFileData file
          else throwE NotDirectory
        (Right dir') ->
          if (xs == []) then throwE NoSuchFileOrDirectory
          else getFileContent dir' xs

information :: FilePath -> ExceptT FSException (State FSState) String
information path = runImmutableFunction getInformation path
  where
    getInformation :: Directory -> [FilePath]
                   -> ExceptT FSException (State FSState) String
    getInformation dir []     = return $ show $ getDirInfo dir
    getInformation dir (x:xs) = do
      a <- lookupInDirectory dir x
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
directoryContent path = runImmutableFunction showContentInDirectory path
  where
    showContentInDirectory :: Directory -> [FilePath]
                           -> ExceptT FSException (State FSState) String
    showContentInDirectory dir [] =
      return $ intercalate ("\n") $ map fst $ Map.toList $ getDirContents dir
    showContentInDirectory dir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE NotDirectory
        (Right dir') -> showContentInDirectory dir' xs

lookupInDirectory :: Directory -> FilePath -> ExceptT FSException (State FSState) DirElement
lookupInDirectory dir name =
  case Map.lookup name (getDirContents dir) of
    Nothing  -> throwE NoSuchFileOrDirectory
    (Just a) -> return a

getCurFSDirectory :: ExceptT FSException (State FSState) Directory
getCurFSDirectory = do
  FSState{fileSystem = fs, curDirectoryPath = pathDir} <- get
  getDir (getRootDirectory fs) (splitDirectories pathDir)
  where
    getDir :: Directory -> [FilePath] -> ExceptT FSException (State FSState) Directory
    getDir dir [] = return dir
    getDir dir (x : xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE FSInconsistent
        (Right dir') -> getDir dir' xs

type ExceptState a = ExceptT FSException (State FSState) a

updateFileSystem :: Directory -> ExceptT FSException (State FSState) ()
updateFileSystem newDir = do
  st@FSState{fileSystem = fs, curDirectoryPath = path}<- get
  let rootDir = getRootDirectory fs
  let splittedPath = splitDirectories path
  newRootDir <- updateDir rootDir newDir splittedPath
  put st{fileSystem = fs{getRootDirectory = newRootDir}}
  return ()
  where
    updateDir :: Directory -> Directory -> [FilePath]
              ->  ExceptT FSException (State FSState) Directory
    updateDir _   newDir []     = return newDir
    updateDir dir newDir (x:xs) = do
      fileOrDir <- lookupInDirectory dir x
      case fileOrDir of
        (Left _    ) -> throwE FSInconsistent
        (Right dir') -> do
          let dirContents = getDirContents dir
          updatedDir <- updateDir dir' newDir xs
          let newDirContents = Map.insert x (Right updatedDir) dirContents
          return dir'{getDirContents = newDirContents}

-- TODO : "../.." и тому подобное
runImmutableFunction :: (Directory -> [FilePath] -> (ExceptState a)) -> FilePath
                     -> (ExceptState a)
runImmutableFunction foo path = do
  FSState{fileSystem = fs} <- get
  if (isAbsolute path) then do
    let splittedPath = tail $ getSplittedPath path
    foo (getRootDirectory fs) splittedPath
  else do
    curDir <- getCurFSDirectory
    let splittedPath = getSplittedPath path
    foo curDir splittedPath

-- Sometimes can "predict" what user meant
getSplittedPath :: FilePath -> [FilePath]
getSplittedPath = splitDirectories . dropTrailingPathSeparator
