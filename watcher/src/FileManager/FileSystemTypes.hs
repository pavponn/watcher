module FileManager.FileSystemTypes where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.List (intercalate)
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import System.Directory.Internal (Permissions(..))
import System.FilePath.Posix ((</>))
import System.IO.Unsafe (unsafePerformIO)

type DirElement = Either File Directory

data FileInfo = FileInfo
                  { getFileType             :: String
                  , getFilePath             :: FilePath
                  , getFileSizeBytes        :: Integer
                  , getFilePermissions      :: Permissions
                  , getFileModificationTime :: UTCTime
                  }

data DirInfo = DirInfo
                 { getDirSize             :: Integer
                 , getDirPath             :: FilePath
                 , getDirPermissions      :: Permissions
                 }

data File = File
              { getFileName :: String
              , getFileInfo :: FileInfo
              , getFileData :: B.ByteString
              }

data Directory = Directory
                   { getDirName     :: String
                   , getDirInfo     :: DirInfo
                   , getDirContents :: Map.Map String DirElement
                   } deriving (Show)

data FileSystem = FileSystem
                    { getRootDirectory       :: Directory
                    , getPathToRootDirectory :: FilePath
                    } deriving (Show)

instance Show File where
  show (File name info _) =
    "File { getFileName = " ++ name  ++ ", getFileInfo = " ++ show info ++ "}"

instance Show FileInfo where
  show file = intercalate "\n"
    [ "Path: " ++ (show $ getFilePath file)
    , "Type: " ++ (show $ getFileType file)
    , "Size: " ++ (show $ getFileSizeBytes file)
    , "Permissions: " ++ (show $ getFilePermissions file)
    , "Modification time: " ++ (show $ getFileModificationTime file)
    ]

instance Show DirInfo where
  show dir = intercalate "\n"
    [ "Path: " ++ (show $ getDirPath dir)
    , "Size: " ++ (show $ getDirSize dir) ++ " bytes"
    , "Permissions: " ++ (show $ getDirPermissions dir)
    ]

-- TODO time
defaultNewFile :: String -> FilePath -> File
defaultNewFile name path = do
  let curTime = unsafePerformIO getCurrentTime
  let fileInfo = FileInfo
                   { getFileType = "X3 4TO ETO"
                   , getFilePath = path </> name
                   , getFileSizeBytes = 0
                   , getFilePermissions = Permissions True True True True
                   , getFileModificationTime = curTime
                   }
  File
    { getFileName = name
    , getFileInfo = fileInfo
    , getFileData = B.empty
    }

-- TODO: standard size
defaultNewDirectory :: String -> FilePath -> Directory
defaultNewDirectory name path = do

  let dirInfo = DirInfo
                  { getDirSize = 200
                  , getDirPath = path </> name
                  , getDirPermissions = Permissions True True True True
                  }
  Directory
    { getDirName = name
    , getDirInfo = dirInfo
    , getDirContents = Map.empty
    }
