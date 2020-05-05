module FileManager.VCSHandlers
  ( initVCS
  , addToVCS
  , showCurVCS
  )where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (isPrefixOf, intercalate, concat)
import Data.Either (lefts, rights)
import qualified Data.Map.Strict as Map
import FileManager.FileSystemTypes
import FileManager.FileSystemUtils
import FileManager.FilePathUtils
import System.FilePath ((</>))
import System.FilePath.Posix (isAbsolute)

type MyMap = Map.Map FilePath (Map.Map Integer (File, String))

showCurVCS :: String -> ExceptT FSException (State FSState) String
showCurVCS _ = do
  vcsPath <- getVCSPath `catchE` (\_ -> throwE $ Message "NO VCS HERE")
  dirVCS <- getDirectoryByPath vcsPath
  storage <- retractVCSStorage dirVCS `catchE` (\_ -> throwE $ Message "WAAAaAAAAAT")
  return $ show $ storage


initVCS :: ExceptT FSException (State FSState) String
initVCS = do
  curDir <- getCurFSDirectory
  case getVCSStorage curDir of
    (Just _a) -> return "VCS was already initialised in current directory"
    Nothing   -> do
      st@FSState{curDirectoryPath = curPath} <- get
      put st{curVCSPath = Just curPath}
      updateFileSystem curPath curDir{getVCSStorage = Just defaultVCSStorage}
      return "VCS was successfully initilised in current directory"

addToVCS :: FilePath -> ExceptT FSException (State FSState) String
addToVCS path = do
  FSState{ curDirectoryPath = curPath} <- get
  pathToVCS <- getVCSPath
  if (isAbsolute path) then
    addToVCSInner pathToVCS path
  else do
    addToVCSInner pathToVCS (curPath </> path)
  where
    addToVCSInner :: FilePath -> FilePath -> ExceptT FSException (State FSState) String
    addToVCSInner vcsPath realPath = do
      normPath <- getNormalisedPath realPath
      if (vcsPath `isPrefixOf` normPath) then do
        fileOrDir <- getDirElementByPath normPath
        case fileOrDir of
          (Left file) -> addFile vcsPath file
          (Right dir) ->  addDir vcsPath dir
      else
        throwE $ UnsupportedOperation "Path to file/directory is not a part of VCS"

addFile :: FilePath -> File -> ExceptT FSException (State FSState) String
addFile vcsPath file = addFiles vcsPath [file]

addDir :: FilePath -> Directory -> ExceptT FSException (State FSState) String
addDir vcsPath dir = do
  allFiles <- getAllFilesInDirAndSubDirs dir
  addFiles vcsPath allFiles

addFiles :: FilePath -> [File] -> ExceptT FSException (State FSState) String
addFiles vcsPath files = do
  dirVCS <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage dirVCS `catchE` (\_ -> throwE $ FSInconsistent)
  let rev = getRevisionsNum storage
  let filesData = getVCSFiles storage
  (newFilesData, msgs, upd) <- addAllToMap files rev filesData [] False
  let newRev = if upd then rev + 1 else rev
  let newStorage = storage{getVCSFiles = newFilesData, getRevisionsNum = newRev}
  let newDir = dirVCS{getVCSStorage = Just newStorage}
  updateFileSystem vcsPath newDir
  return $ intercalate "\n" msgs



addAllToMap :: [File] -> Integer -> MyMap -> [String] -> Bool -> ExceptT FSException (State FSState) (MyMap, [String], Bool)
addAllToMap []     _   filesData outputs upd = return (filesData, outputs, upd)
addAllToMap (f:fs) rev filesData outputs upd= do
  (newFilesData, output, newUpd) <- addToMap f rev filesData
  addAllToMap fs rev newFilesData (output:outputs) (upd || newUpd)

addToMap :: File -> Integer -> MyMap -> ExceptT FSException (State FSState) (MyMap, String, Bool)
addToMap file rev filesData = do
  let absPath = getFilePath $ getFileInfo file
  if (Map.member absPath filesData) then
    return (filesData, "File is already in CVS: " ++ absPath, False)
  else do
    let newFilesData = Map.insert absPath (Map.singleton rev (file, "initial")) filesData
    return (newFilesData, "Added file: " ++ absPath, True)

getAllFilesInDirAndSubDirs :: Directory -> ExceptT FSException (State FSState) [File]
getAllFilesInDirAndSubDirs curDir = do
  let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents curDir
  filesInSubDir <- mapM getAllFilesInDirAndSubDirs (rights dirElements)
  return $ (lefts dirElements) ++  (concat filesInSubDir)
