module FileManager.VCSHandlers
  ( initVCS
  , addToVCS
  , showCurVCS
  , updateInVCS
  , fileHistoryVCS
  , fileVersionVCS
  , allHistoryVCS
  , removeFromVCS
  )where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.List (concat, intercalate, isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import FileManager.FilePathUtils
import FileManager.FileSystemTypes
import FileManager.FileSystemUtils
import System.FilePath ((</>))
import System.FilePath.Posix (isAbsolute)

type VCSMap = Map.Map FilePath (Map.Map Integer (File, String))

showCurVCS :: String -> ExceptState String
showCurVCS _ = do
  vcsPath <- getVCSPath `catchE` (\_ -> throwE $ Message "NO VCS HERE")
  dirVCS <- getDirectoryByPath vcsPath
  storage <- retractVCSStorage dirVCS `catchE` (\_ -> throwE $ Message "WAAAaAAAAAT")
  return $ show $ storage

-- | Initializes VCS in current directory, if VCS is already initialize does nothing.
-- Returns message of operration status. Updates state (file system,
-- path to current VCS directory).
initVCS :: ExceptState String
initVCS = do
  curDir <- getCurFSDirectory
  case getVCSStorage curDir of
    (Just _a) -> return "VCS was already initialised in current directory"
    Nothing   -> do
      st@FSState{curDirectoryPath = curPath} <- get
      put st{curVCSPath = Just curPath}
      updateFileSystem curPath curDir{getVCSStorage = Just defaultVCSStorage}
      return "VCS was successfully initilised in current directory"

-- | Adds file/directory (all files in directory and its subdirectories)
-- that coresponds to provided path to current VCS.
-- Updates state (file system). Throws `ImpossibleToPerform` if current directory
-- is not a part of VCS or filepath is outside of it, NoSuchFileOrDirectory`
-- if there is no such file, `NotValidPath` if path is invalid.
addToVCS :: FilePath -> ExceptState String
addToVCS path = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    addToVCSInner path
  else do
    addToVCSInner (curPath </> path)
  where
    addToVCSInner :: FilePath -> ExceptState String
    addToVCSInner realPath = do
      vcsPath <- getVCSPath
      normPath <- getNormalisedPath realPath
      if (vcsPath `isPrefixOf` normPath) then do
        fileOrDir <- getDirElementByPath normPath
        case fileOrDir of
          (Left file) -> addFile vcsPath file
          (Right dir) ->  addDir vcsPath dir
      else
        throwE $ ImpossibleToPerform "path to file/directory is not a part of VCS"

-- | Updates file that coresponds to provided path in VCS with message.
-- Returns string info on operation status. Throws `VCSException` if path is a path
-- to directory, `ImpossibleToPerform` if provided  file can't be tracked by
-- VCS/or there is no VCS in current directory, NoSuchFileOrDirectory`
-- if there is no such file, `NotValidPath` if path is invalid.
updateInVCS :: (FilePath, String) -> ExceptState String
updateInVCS (path, message) = do
    FSState{curDirectoryPath = curPath} <- get
    if (isAbsolute path) then
      updateInVCSInner path message
    else do
      updateInVCSInner (curPath </> path) message
    where
      updateInVCSInner :: FilePath -> String -> ExceptState String
      updateInVCSInner realPath msg = do
        vcsPath <- getVCSPath
        normPath <- getNormalisedPath realPath
        if (vcsPath `isPrefixOf` normPath) then do
          fileOrDir <- getDirElementByPath normPath
          case fileOrDir of
            (Left file) -> updateFile vcsPath file msg
            (Right _  ) -> throwE $ VCSException "can't update directories"
        else
          throwE $ ImpossibleToPerform "path to file/directory is not a part of VCS"

-- | Removes file from VCS and returns operation's status message.
--  Updates state. Throws `VCSException` if there is no such file in VCS,
-- `NotValidPath` if path is invalid, `ImpossibleToPerform`
-- if current directory isn't a part of VCS.
removeFromVCS :: FilePath -> ExceptState String
removeFromVCS path = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    removeFromVCSInner path
  else do
    removeFromVCSInner (curPath </> path)
  where
    removeFromVCSInner :: FilePath -> ExceptState String
    removeFromVCSInner realPath = do
      FSState{curFileSystem = fs} <- get
      vcsPath <- getVCSPath
      vcsDir <- getDirectoryByPath vcsPath
      storage <- retractVCSStorage vcsDir
      normPath <- getNormalisedPath realPath
      let filesData = getVCSFiles storage
      let absPath = (getPathToRootDirectory fs) </> normPath
      if (Map.member absPath filesData) then do
        let newFilesData = Map.delete absPath filesData
        let newDir = vcsDir{getVCSStorage = Just storage{getVCSFiles = newFilesData}}
        updateFileSystem vcsPath newDir
        return $ "Deleted file from VCS: " ++ absPath
      else
        throwE $ VCSException $ "no such file in VCS: " ++ absPath

-- | Returns whole VCS history in chronological order (as a String).
-- Throws `ImpossibleToPerform` if current directory is not a part of VCS.
allHistoryVCS :: ExceptState String
allHistoryVCS = do
  vcsPath <- getVCSPath
  vcsDir <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage vcsDir  `catchE` (\_ -> throwE $ FSInconsistent)
  let maps = Map.elems $ getVCSFiles storage
  let list = sort $ map (\(i, (f, m)) -> (i, m, getFilePath $ getFileInfo f)) $
                      concat $ map Map.toList maps
  return $ intercalate "\n" $
            map (\(i, m, n) -> (show i) ++ ". " ++ m ++ " (" ++ n ++ ")") list

-- | Accepts path to file and returns it's history from VCS in chronological order
-- (as a String). Throws `ImpossibleToPerform` if current directory is not a part of VCS,
-- `VCSException` if there is no such file in VCS.
fileHistoryVCS :: FilePath -> ExceptState String
fileHistoryVCS path = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    fileHistoryVCSInner path
  else do
    fileHistoryVCSInner (curPath </> path)
  where
    fileHistoryVCSInner :: FilePath -> ExceptState String
    fileHistoryVCSInner realPath = do
      vcsPath <- getVCSPath
      fileHistory <- getFileHistory vcsPath realPath
      let historyList = sort $  map (\(r, (_, s)) -> (r, s)) $ Map.toList fileHistory
      return $ intercalate "\n" $ map (\(r, s) -> (show r) ++ ". " ++ s) historyList

-- | Accepts path to file and revision's index, return file's content for specified
-- index. Throws `ImpossibleToPerform` if current directory is not a part of VCS,
-- `VCSException` if there is no such file in VCS or there is no revision with such index.
fileVersionVCS :: (FilePath, Integer) -> ExceptState B.ByteString
fileVersionVCS (path, index) = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    fileVersionVCSInner path
  else do
    fileVersionVCSInner (curPath </> path)
  where
    fileVersionVCSInner :: FilePath -> ExceptState B.ByteString
    fileVersionVCSInner realPath = do
      vcsPath <- getVCSPath
      fileHistory <- getFileHistory vcsPath realPath
      case Map.lookup index fileHistory of
        (Just f) -> do
          return $ (getFileData . fst) f
        Nothing  -> do
          throwE $ VCSException $ "no revision with index " ++ (show index) ++
                                    " found for specified file"

updateFile :: FilePath -> File -> String -> ExceptState String
updateFile vcsPath file msg = do
  dirVCS <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage dirVCS `catchE` (\_ -> throwE $ FSInconsistent)
  let rev = getRevisionsNum storage
  let filesData = getVCSFiles storage
  newFilesData <- updateMap file msg rev filesData
  let newStorage = storage{getVCSFiles = newFilesData, getRevisionsNum = rev + 1}
  let newDir = dirVCS{getVCSStorage = Just newStorage}
  updateFileSystem vcsPath newDir
  return $ "Updated file: " ++ (getFilePath $ getFileInfo file)

addFile :: FilePath -> File -> ExceptState String
addFile vcsPath file = addFiles vcsPath [file]

addDir :: FilePath -> Directory -> ExceptState String
addDir vcsPath dir = do
  allFiles <- getAllFilesInDirAndSubDirs dir
  addFiles vcsPath allFiles

addFiles :: FilePath -> [File] -> ExceptState String
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

updateMap :: File -> String -> Integer -> VCSMap -> ExceptState VCSMap
updateMap file msg rev filesData = do
  let absPath = getFilePath $ getFileInfo file
  case (Map.lookup absPath filesData) of
    Nothing ->
     throwE $ ImpossibleToPerform $ "file " ++ absPath ++ "isn't in VCS"
    (Just revMap) -> do
      let newRevMap = Map.insert rev (file,msg) revMap
      let newFilesData = Map.insert absPath newRevMap filesData
      return newFilesData

addAllToMap :: [File] -> Integer -> VCSMap -> [String] -> Bool
            -> ExceptState (VCSMap, [String], Bool)
addAllToMap []     _   filesData outputs upd = return (filesData, outputs, upd)
addAllToMap (f:fs) rev filesData outputs upd = do
  (newFilesData, output, newUpd) <- addToMap f rev filesData
  addAllToMap fs rev newFilesData (output:outputs) (upd || newUpd)

addToMap :: File -> Integer -> VCSMap -> ExceptState (VCSMap, String, Bool)
addToMap file rev filesData = do
  let absPath = getFilePath $ getFileInfo file
  if (Map.member absPath filesData) then
    return (filesData, "File is already in VCS: " ++ absPath, False)
  else do
    let newFilesData = Map.insert absPath (Map.singleton rev (file, "initial")) filesData
    return (newFilesData, "Added file: " ++ absPath, True)

getFileHistory :: FilePath -> FilePath -> ExceptState (Map.Map Integer (File, String))
getFileHistory vcsPath realPath = do
  vcsDir <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage vcsDir `catchE` (\_ -> throwE $ FSInconsistent)
  normPath <- getNormalisedPath realPath
  FSState{curFileSystem = fs} <- get
  let absPath = (getPathToRootDirectory fs) </> normPath
  case Map.lookup absPath (getVCSFiles storage) of
    (Just history) -> return $ history
    Nothing        -> throwE $ VCSException "no such file in current VCS"
