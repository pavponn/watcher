module FileManager.FileSystemTypes where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Data.Time.Clock(UTCTime(..))
import System.Directory(Permissions(..))
import Data.Map
import Data.List(intercalate)

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
                 , getDirModificationTime :: UTCTime
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
                    { getRootDirectory :: Directory
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
    , "Modification time: " ++ (show $ getDirModificationTime dir)
    ]
