module Utils.LoaderUtils
  ( isDirectory
  , isFile
  , listExceptionHandler
  , permsExceptionHandler
  , falseExceptionHandler
  , rethrowHandler
  , maybeExceptionHandler
  , timeExceptionHandler
  , itsOkayExceptionHandler
  , defaultTimeUTC
  ) where

import Control.Exception (SomeException, catch, throw)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import System.Directory.Internal (Permissions (..))
import System.FilePath.Posix ((</>))

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

defaultTimeUTC :: UTCTime
defaultTimeUTC = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

itsOkayExceptionHandler :: SomeException -> IO ()
itsOkayExceptionHandler = \_ -> return ()

timeExceptionHandler :: SomeException -> IO UTCTime
timeExceptionHandler = \_ -> return $ defaultTimeUTC

maybeExceptionHandler :: SomeException -> IO (Maybe a)
maybeExceptionHandler = \_ -> return Nothing

rethrowHandler :: SomeException -> IO a
rethrowHandler = \ex -> throw ex

listExceptionHandler :: SomeException -> IO [FilePath]
listExceptionHandler = \_ -> return []

permsExceptionHandler :: SomeException -> IO Permissions
permsExceptionHandler = \_ -> return $ Permissions False False False False

falseExceptionHandler :: SomeException -> IO Bool
falseExceptionHandler = \_ -> return False
