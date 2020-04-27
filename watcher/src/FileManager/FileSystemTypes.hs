module FileManager.FileSystemTypes where

import qualified Data.ByteString as B
import Data.Time.Clock(UTCTime(..))
import System.Directory(Permissions(..))

type DirElement = Either File Directory

data FileInfo = FileInfo
                  { getFileType             :: String
                  , getFileSizeBytes        :: Integer
                  , getFilePermissions      :: Permissions
                  , getFileModificationTime :: UTCTime
                  } deriving (Show)

data DirInfo = DirInfo
                 { getDirSize             :: Integer
                 , getDirPermissions      :: Permissions
                 , getDirModificationTime :: UTCTime
                 } deriving (Show)

data File = File
              { getFileName :: String
              , getFileInfo :: FileInfo
              , getFileData :: B.ByteString
              }

data Directory = Directory
                   { getDirName     :: String
                   , getDirInfo     :: DirInfo
                   , getDirContents :: [DirElement]
                   } deriving (Show)

data FileSystem = FileSystem
                    { getRootDirectory :: Directory
                    , getPathToRootDirectory :: FilePath
                    } deriving (Show)

instance Show File where
  show (File name info _) =
    "File { getFileName = " ++ name  ++ ", getFileInfo = " ++ show info ++ "}"

--
-- Directory {
--   getDirName = "/Users/pavel/ITMO/functional-programming/hw2-pavponn/src",
--   getDirContents = [
--     Right (Directory
--           { getDirName = "1",
--             getDirContents = [
--              Right (Directory
--                    { getDirName = "3",
--                      getDirContents = [
--                      Right (Directory
--                             { getDirName = "4",
--                               getDirContents = []
--                             })
--                      ]
--                    }
--                    )
--              ]
--           }),
--     Right (Directory
--           { getDirName = "2",
--             getDirContents = [
--               Right (Directory
--                     { getDirName = "5",
--                       getDirContents = [
--                         Right (Directory {
--                               getDirName = "6",
--                               getDirContents = []
--                               }
--                               )
--                       ]
--                    }
--                    )
--            ]
--         }
--     )
--     ]
-- }
