module FileManager.VCSHandlers
  ( initVCS
  , addToVCS
  , showCurVCS
  , updateInVCS
  , fileHistoryVCS
  , fileVersionVCS
  , allHistoryVCS
  )where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import Data.Either (lefts, rights)
import Data.List (concat, intercalate, isPrefixOf, sort)
import qualified Data.Map.Strict as Map
import FileManager.FilePathUtils
import FileManager.FileSystemTypes
import FileManager.FileSystemUtils
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
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    addToVCSInner path
  else do
    addToVCSInner (curPath </> path)
  where
    addToVCSInner :: FilePath -> ExceptT FSException (State FSState) String
    addToVCSInner realPath = do
      vcsPath <- getVCSPath
      normPath <- getNormalisedPath realPath
      if (vcsPath `isPrefixOf` normPath) then do
        fileOrDir <- getDirElementByPath normPath
        case fileOrDir of
          (Left file) -> addFile vcsPath file
          (Right dir) ->  addDir vcsPath dir
      else
        throwE $ ImpossibleToPerform "Path to file/directory is not a part of VCS"

updateInVCS :: (FilePath, String) -> ExceptT FSException (State FSState) String
updateInVCS (path, message) = do
    FSState{curDirectoryPath = curPath} <- get
    if (isAbsolute path) then
      updateInVCSInner path message
    else do
      updateInVCSInner (curPath </> path) message
    where
      updateInVCSInner :: FilePath -> String -> ExceptT FSException (State FSState) String
      updateInVCSInner realPath msg = do
        vcsPath <- getVCSPath
        normPath <- getNormalisedPath realPath
        if (vcsPath `isPrefixOf` normPath) then do
          fileOrDir <- getDirElementByPath normPath
          case fileOrDir of
            (Left file) -> updateFile vcsPath file msg
            (Right _  ) -> throwE $ UnsupportedOperation "Can't update directories"
        else
          throwE $ ImpossibleToPerform "Path to file/directory is not a part of VCS"

allHistoryVCS :: ExceptT FSException (State FSState) String
allHistoryVCS = do
  vcsPath <- getVCSPath
  vcsDir <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage vcsDir  `catchE` (\_ -> throwE $ FSInconsistent)
  let maps = Map.elems $ getVCSFiles storage
  let list = sort $ map (\(i, (f, m)) -> (i, m, getFilePath $ getFileInfo f)) $
                      concat $ map Map.toList maps
  return $ intercalate "\n" $
            map (\(i, m, n) -> (show i) ++ ". " ++ m ++ "(" ++ n ++ ") ") list


fileHistoryVCS :: FilePath -> ExceptT FSException (State FSState) String
fileHistoryVCS path = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    fileHistoryVCSInner path
  else do
    fileHistoryVCSInner (curPath </> path)
  where
    fileHistoryVCSInner :: FilePath -> ExceptT FSException (State FSState) String
    fileHistoryVCSInner realPath = do
      vcsPath <- getVCSPath
      fileHistory <- getFileHistory vcsPath realPath
      let historyList = sort $  map (\(r, (_, s)) -> (r, s)) $ Map.toList fileHistory
      return $ intercalate "\n" $ map (\(r, s) -> (show r) ++ ". " ++ s) historyList

fileVersionVCS :: (FilePath, Integer)
               -> ExceptT FSException (State FSState) B.ByteString
fileVersionVCS (path, index) = do
  FSState{curDirectoryPath = curPath} <- get
  if (isAbsolute path) then
    fileVersionVCSInner path
  else do
    fileVersionVCSInner (curPath </> path)
  where
    fileVersionVCSInner :: FilePath -> ExceptT FSException (State FSState) B.ByteString
    fileVersionVCSInner realPath = do
      vcsPath <- getVCSPath
      fileHistory <- getFileHistory vcsPath realPath
      case Map.lookup index fileHistory of
        (Just f) -> do
          return $ (getFileData . fst) f
        Nothing  -> do
          throwE $ VCSException $ "No revision with index " ++ (show index) ++
                                    " found for specified file "

updateFile :: FilePath -> File -> String -> ExceptT FSException (State FSState) String
updateFile vcsPath file msg = do
  dirVCS <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage dirVCS `catchE` (\_ -> throwE $ FSInconsistent)
  let rev = getRevisionsNum storage
  let filesData = getVCSFiles storage
  newFilesData <- updateMap file msg rev filesData
  let newStorage = storage{getVCSFiles = newFilesData, getRevisionsNum = rev + 1}
  let newDir = dirVCS{getVCSStorage = Just newStorage}
  updateFileSystem vcsPath newDir
  return $ "File " ++  (getFilePath $ getFileInfo file ) ++ " updated in VCS."

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

updateMap :: File -> String -> Integer -> MyMap
          -> ExceptT FSException (State FSState) MyMap
updateMap file msg rev filesData = do
  let absPath = getFilePath $ getFileInfo file
  case (Map.lookup absPath filesData) of
    Nothing ->
     throwE $ ImpossibleToPerform $ "File " ++ absPath ++ "isn't in VCS"
    (Just revMap) -> do
      let newRevMap = Map.insert rev (file,msg) revMap
      let newFilesData = Map.insert absPath newRevMap filesData
      return newFilesData

addAllToMap :: [File] -> Integer -> MyMap -> [String] -> Bool
            -> ExceptT FSException (State FSState) (MyMap, [String], Bool)
addAllToMap []     _   filesData outputs upd = return (filesData, outputs, upd)
addAllToMap (f:fs) rev filesData outputs upd = do
  (newFilesData, output, newUpd) <- addToMap f rev filesData
  addAllToMap fs rev newFilesData (output:outputs) (upd || newUpd)

addToMap :: File -> Integer -> MyMap
         -> ExceptT FSException (State FSState) (MyMap, String, Bool)
addToMap file rev filesData = do
  let absPath = getFilePath $ getFileInfo file
  if (Map.member absPath filesData) then
    return (filesData, "File is already in VCS: " ++ absPath, False)
  else do
    let newFilesData = Map.insert absPath (Map.singleton rev (file, "initial")) filesData
    return (newFilesData, "Added file: " ++ absPath, True)

getFileHistory :: FilePath -> FilePath
              -> ExceptT FSException (State FSState) (Map.Map Integer (File, String))
getFileHistory vcsPath realPath = do
  vcsDir <- getDirectoryByPath vcsPath `catchE` (\_ -> throwE $ FSInconsistent)
  storage <- retractVCSStorage vcsDir `catchE` (\_ -> throwE $ FSInconsistent)
  normPath <- getNormalisedPath realPath
  FSState{curFileSystem = fs} <- get
  let absPath = (getPathToRootDirectory fs) </> normPath
  case Map.lookup absPath (getVCSFiles storage) of
    (Just history) -> return $ history
    Nothing        -> throwE $ VCSException "No such file in current VCS"

getAllFilesInDirAndSubDirs :: Directory -> ExceptT FSException (State FSState) [File]
getAllFilesInDirAndSubDirs curDir = do
  let dirElements = map (\x -> snd x) $ Map.toList $ getDirContents curDir
  filesInSubDir <- mapM getAllFilesInDirAndSubDirs (rights dirElements)
  return $ (lefts dirElements) ++ (concat filesInSubDir)
