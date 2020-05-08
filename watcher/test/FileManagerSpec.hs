module FileManagerSpec where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map.Strict as Map
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import FileSystemTypes
import Handlers.FileManagerHandlers
import System.Directory.Internal (Permissions (..))
import Test.Hspec (SpecWith, describe, it, shouldBe, shouldContain, shouldNotBe, shouldNotContain)

fullSpec :: SpecWith ()
fullSpec = do
  changeDirSpec
  removeSpec
  createDirectorySpec
  createFileSpec
  findFileSpec
  writeToFileSpec
  fileContentSpec
  informationSpec
  directoryContentSpec

changeDirSpec :: SpecWith ()
changeDirSpec = do
  describe "FileManagerSpec.changeDirSpec" $ do
    it "should change directory" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory "folder") st
      res `shouldBe` (Right ())
      (curDirectoryPath newState) `shouldBe` "folder"
    it "should change directory with abs path" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory "/folder") st
      res `shouldBe` (Right ())
      (curDirectoryPath newState) `shouldBe` "folder"
    it "should change directory with .. paths" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory "/folder/../folder") st
      res `shouldBe` (Right ())
      (curDirectoryPath newState) `shouldBe` "folder"
    it "should go back" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory "folder") st
      res `shouldBe` (Right ())
      (curDirectoryPath newState) `shouldBe` "folder"
      let (res2, _) = runState (runExceptT $ goToDirectory "..") newState
      res2 `shouldBe`(Right ())
      (curDirectoryPath newState) `shouldBe` "folder"
    it "shouldn't go back from root" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory "..") st
      res `shouldNotBe` (Right ())
      (curDirectoryPath newState) `shouldBe` ""
    it "shouldn't change directroy" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ goToDirectory ".") st
      res `shouldBe` (Right ())
      (curDirectoryPath newState) `shouldBe` ""

removeSpec :: SpecWith ()
removeSpec = do
  describe "FileManagerSpec.removeSpec" $ do
    it "should remove file" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ removeFileOrDirectory "a.md") st
      res `shouldBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldNotContain` ["a.md"]
    it "should remove directory" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ removeFileOrDirectory "folder") st
      res `shouldBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldNotContain` ["folder"]
    it "shouldn't remove root folder folder" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ removeFileOrDirectory ".") st
      res `shouldNotBe` (Right ())

createDirectorySpec :: SpecWith ()
createDirectorySpec = do
  describe "FileManagerSpec.createDirectorySpec" $ do
    it "should create directory" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ createDirectory "dir") st
      res `shouldBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldContain` ["dir"]
    it "shouldn't create .vcs directory" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ createDirectory "folder") st
      res `shouldNotBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldNotContain` [".vcs"]

createFileSpec :: SpecWith ()
createFileSpec = do
  describe "FileManagerSpec.createFileSpec" $ do
    it "should create file" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ createFile ("a.txt", defaultTimeUTC)) st
      res `shouldBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldContain` ["a.txt"]
    it "shouldn't create .vcs file" $ do
      let st = testState
      let (res, newState) = runState (runExceptT $ createFile (".vcs", defaultTimeUTC)) st
      res `shouldNotBe` (Right ())
      (contentNames $ getRootDirectory $ curFileSystem newState) `shouldNotContain` [".vcs"]

findFileSpec :: SpecWith ()
findFileSpec = do
  describe "FileManagerSpec.findFileSpec" $ do
    it "should find file" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ findFile "mda") st
      res `shouldNotBe` (Left FileNotFound)
    it "shouldn't find file" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ findFile "that-doesnt-exist") st
      res `shouldBe` (Left FileNotFound)
    it "shouldn't find directory" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ findFile "inner") st
      res `shouldBe` (Left FileNotFound)

writeToFileSpec :: SpecWith ()
writeToFileSpec = do
  describe "FileManagerSpec.writeToFileSpec" $ do
    it "should write to file" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ writeToFile (BS.fromString "content", "mda", defaultTimeUTC)) st
      res `shouldBe` (Right ())
    it "shouldn't write to file" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ writeToFile (BS.fromString "content", "nds", defaultTimeUTC)) st
      res `shouldNotBe` (Right ())

fileContentSpec :: SpecWith ()
fileContentSpec = do
  describe "FileManagerSpec.fileContentSpec" $ do
    it "should get file content" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ fileContent "mda") st
      res `shouldBe` (Right (BS.fromString "/Users/pavel/small-test-dir/mda"))
    it "shouldn't get file content" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ fileContent "fhsd.txt") st
      res `shouldBe` (Left NoSuchFileOrDirectory)

informationSpec :: SpecWith ()
informationSpec = do
  describe "FileManagerSpec.informationSpec" $ do
    it "should get file information" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ information "mda") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "should get current dir information" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ information ".") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "should get dir information" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ information "folder") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "shouldn't get information" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ information "folder.mda") st
      res `shouldBe` (Left NoSuchFileOrDirectory)

directoryContentSpec :: SpecWith ()
directoryContentSpec = do
  describe "FileManagerSpec.directoryContentSpec" $ do
    it "should get current folder content #1" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ directoryContent ".") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "should get current folder content #2" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ directoryContent "") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "should get dir content" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ directoryContent "folder") st
      res `shouldNotBe` (Left NoSuchFileOrDirectory)
    it "shouldn't get dir content #1" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ directoryContent "aaaa") st
      res `shouldBe` (Left NoSuchFileOrDirectory)
    it "shouldn't get dir content #2" $ do
      let st = testState
      let (res, _) = runState (runExceptT $ directoryContent "mda/") st
      res `shouldBe` (Left NoSuchFileOrDirectory)

testState :: FSState
testState = FSState testFileSystem "" Nothing

contentNames :: Directory -> [String]
contentNames Directory{getDirContents = contents} = map fst $ Map.toList contents

testFileSystem :: FileSystem
testFileSystem = FileSystem
                   { getRootDirectory = stDir "small-test-dir"  "/Users/pavel/small-test-dir" (Map.fromList
                    [("folder", Right (stDir "folder" "/Users/pavel/small-test-dir/folder" (Map.fromList []))),
                    ("inner", Right (stDir "inner" "/Users/pavel/small-test-dir/inner" $ Map.fromList
                        [("a.md", Left (stFile "a.md" "/Users/pavel/small-test-dir/inner/a.md"))]))
                    ,("mda", Left (stFile "mda" "/Users/pavel/small-test-dir/mda"))])
                  , getPathToRootDirectory = "/Users/pavel/small-test-dir"}

stDirInfo :: FilePath -> DirInfo
stDirInfo path = DirInfo
                   { getDirSize = 160
                   , getDirPath = path
                   , getDirPermissions = stDirPermissions
                   }

stFileInfo :: FilePath -> FileInfo
stFileInfo path = FileInfo
                    { getFileTypes = []
                    , getFilePath  = path
                    , getFileSizeBytes = 12
                    , getFilePermissions = stFilePermissions
                    , getFileModificationTime = defaultTimeUTC
                    }

stDir :: String -> FilePath ->  Map.Map String DirElement -> Directory
stDir name path content = Directory
                            { getDirName = name
                            , getDirInfo = stDirInfo path
                            , getDirContents = content
                            , getVCSStorage = Nothing
                            }

stFile :: String -> FilePath -> File
stFile name path = File
                     { getFileName = name
                     , getFileInfo = stFileInfo path
                     , getFileData = BS.fromString path
                     }

defaultTimeUTC :: UTCTime
defaultTimeUTC = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

stDirPermissions :: Permissions
stDirPermissions = Permissions True True False True

stFilePermissions :: Permissions
stFilePermissions = Permissions True True False False
