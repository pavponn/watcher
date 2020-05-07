module Utils.LoaderUtils
  ( isDirectory
  , isFile
  , listExceptionHandler
  , permsExceptionHandler
  , falseExceptionHandler
  ) where

import Control.Exception (SomeException, catch)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.FilePath.Posix ((</>))
import System.Directory.Internal (Permissions (..))

-- | Checks whether given filepath is a directory. If it's impossible due to
-- some reason, returns False.
isDirectory :: FilePath -> FilePath -> IO Bool
isDirectory path name = do
  let realPath = path </> name
  isDir <- doesDirectoryExist realPath `catch` falseExceptionHandler
  isSymbLink <- pathIsSymbolicLink realPath `catch` falseExceptionHandler
  return $ isDir && not isSymbLink

-- | Checks whether given filepath is a file. If it's impossible due to
-- some reason, returns False.
isFile :: FilePath -> FilePath -> IO Bool
isFile path name = do
  let realPath = path </> name
  isF <- doesFileExist realPath `catch` falseExceptionHandler
  isSymbLink <- pathIsSymbolicLink realPath `catch` falseExceptionHandler
  return $ isF && not isSymbLink

listExceptionHandler :: SomeException -> IO [FilePath]
listExceptionHandler = \_ -> return []

permsExceptionHandler :: SomeException -> IO Permissions
permsExceptionHandler = \_ -> return $ Permissions False False False False

falseExceptionHandler :: SomeException -> IO Bool
falseExceptionHandler = \_ -> return False
