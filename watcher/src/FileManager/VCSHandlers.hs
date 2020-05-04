module FileManager.VCSHandlers
  ( initVCS
  )where

import Control.Monad.State
import Control.Monad.Trans.Except
import FileManager.FileSystemTypes
import FileManager.FileSystemUtils

initVCS :: ExceptT FSException (State FSState) String
initVCS = do
  curDir <- getCurFSDirectory
  case getVCSStorage curDir of
    (Just _a) -> return "VCS was already initialised in current directory"
    Nothing  -> do
      st@FSState{curDirectoryPath = curPath} <- get
      put st{curVCSPath = Just curPath}
      updateFileSystem curPath curDir{getVCSStorage = Just defaultVCSStorage}
      return "VCS was successfully initilised in current directory"

-- addToVCS :: FilePath -> ExceptT FSException (State FSState) String
-- addToVCS path = do
--   FSState{curVCSPath = maybeVCSPath} <- get
--   throwIfNotInCVS
--   if (isAbsolute path) then
--     addToVCSInner path
--   else do
--     FSState{curDirectoryPath = curPath} <- get
--     addToVCSInner $ curPath </> path
--   where
--     addToVCSInner realPath = do


-- throwIfNotInCVS :: Maybe FilePath -> ExceptT FSException (State FSState) ()
-- throwIfNotInCVS Nothing =
--   throwE $ UnsupportedOperation "Current directory is not a part of VCS"
-- throwIfNotInCVS _ = return ()
