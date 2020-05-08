module FileSystemTypes where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (intercalate)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..))
import Network.Mime (fileNameExtensions)
import System.Directory.Internal (Permissions (..))
import System.FilePath.Posix ((</>))

-- |Type alias for elements in directory. Element in directory is
-- either File or Directory.
type DirElement = Either File Directory

-- | Data type that stores all useful information about file.
data FileInfo = FileInfo
  { getFileTypes            :: [String]
  , getFilePath             :: FilePath
  , getFileSizeBytes        :: Integer
  , getFilePermissions      :: Permissions
  , getFileModificationTime :: UTCTime
}

-- | Data type that stores all useful information about directory.
data DirInfo = DirInfo
  { getDirSize        :: Integer
  , getDirPath        :: FilePath
  , getDirPermissions :: Permissions
  }

-- | Data type that represents file.
data File = File
  { getFileName :: String
  , getFileInfo :: FileInfo
  , getFileData :: B.ByteString
  }

-- | Data type that represents directory.
data Directory = Directory
  { getDirName     :: String
  , getDirInfo     :: DirInfo
  , getDirContents :: Map.Map String DirElement
  , getVCSStorage  :: Maybe VCSStorage
  } deriving (Show)

-- | Data type that represents VCS. It stores number of revisions made by
-- in VCS (globally) as 'numberOfRevisions', and files that are already in
-- added to VCS with all their revisions.
data VCSStorage = VCSStorage
  { getVCSFiles     :: Map.Map FilePath (Map.Map Integer (File, String))
  , getRevisionsNum :: Integer
  }

-- | Represents current FileSystem as root directory and path to it in a real
-- file system.
data FileSystem = FileSystem
  { getRootDirectory       :: Directory
  , getPathToRootDirectory :: FilePath
  } deriving (Show)

-- | Stores not only the file system, but also path to current directory (as a
-- relative path to root directory: if we are in the root, than "" is stored.
-- If current directory is a part of some VCS, contains 'Just path' to directory where VCS
-- was initalised, otherwise contains 'Nothing'.
data FSState = FSState
  { curFileSystem    :: FileSystem
  , curDirectoryPath :: FilePath
  , curVCSPath       :: Maybe FilePath
  } deriving (Show)

-- | Represents exceptions that can occur while working with file system.
data FSException
  = NotFile
  | NotDirectory
  | FileNotFound
  | NotValidName String
  | VCSException String
  | NotValidPath FilePath
  | NoSuchFileOrDirectory
  | PermissionsDenied FilePath
  | UnsupportedOperation String
  | DuplicateFileOrDirectory String
  | FSInconsistent

-- | Typealias for monad we're working in. Just to make it shorter.
type ExceptState a = ExceptT FSException (State FSState) a

instance Show File where
  show (File name info _) =
    "File { getFileName = " ++ name  ++ ", getFileInfo = " ++ show info ++ "}"

instance Show FileInfo where
  show file = intercalate "\n"
    [ "Path: " ++ (show $ getFilePath file)
    , "Size: " ++ (show $ getFileSizeBytes file) ++ " bytes"
    , "Types: " ++ (intercalate ", " $ getFileTypes file)
    , "Permissions: " ++ (customPermissionsShow $ getFilePermissions file)
    , "Modification time: " ++ (show $ getFileModificationTime file)
    ]

instance Show DirInfo where
  show dir = intercalate "\n"
    [ "Path: " ++ (show $ getDirPath dir)
    , "Size: " ++ (show $ getDirSize dir) ++ " bytes"
    , "Permissions: " ++ (customPermissionsShow $ getDirPermissions dir)
    ]

instance Show VCSStorage where
  show storage = (show $ getRevisionsNum storage) ++ (show $ getVCSFiles storage)

instance Show FSException where
  show NotFile = "not a path to file."
  show NotDirectory = "not a path to directory."
  show FileNotFound = "there is no such file in directory with matching name."
  show (NotValidName name) = "invalid name \"" ++ name ++ "\"."
  show (NotValidPath path) = "path" ++  path ++ " is invalid.\n" ++
    "There might be a problem in \"..\". Remember, watcher treats it's main directory" ++
    "as a root and knows nothing else about your real file system that is out of it."
  show (VCSException msg) = msg
  show NoSuchFileOrDirectory = "no such file or directory."
  show (PermissionsDenied path) = "Permission to " ++ path ++ " denied."
  show (UnsupportedOperation msg) = "unsuppored operation (" ++ msg ++ ")."
  show (DuplicateFileOrDirectory name) = "file or directory with name " ++ name ++ "already exists."
  show FSInconsistent = "that's really sad. May you please provide sequence of" ++
    "actions that has leaded to this result?"

-- | Returns file with specified name and path in `FileInfo`.
defaultNewFile :: String -> FilePath -> UTCTime -> File
defaultNewFile name path curTime = do
  let fileTypes = T.unpack <$> (fileNameExtensions . T.pack) name
  let perms = Permissions True True False False
  let fileInfo = FileInfo fileTypes (path </> name) 0 perms curTime
  File name fileInfo B.empty

-- | Returns new directory with specified name and path in `DirInfo`.
defaultNewDirectory :: String -> FilePath -> Directory
defaultNewDirectory name path = do
  let dirInfo = DirInfo 200 (path </> name) (Permissions True True False True)
  Directory name dirInfo Map.empty Nothing

-- | Returns empty VCSStorage.
defaultVCSStorage :: VCSStorage
defaultVCSStorage = VCSStorage Map.empty 0

customPermissionsShow :: Permissions -> String
customPermissionsShow (Permissions r w e s) =
  "readable = " ++ (show r) ++ ", writable = " ++ (show w) ++
    ", executable = " ++ (show e) ++ ", searchable = " ++ (show s)
