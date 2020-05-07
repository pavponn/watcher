module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as B
import Data.Semigroup ((<>))
import Data.Time.Clock (getCurrentTime)
import FileManager.FileManagerHandlers (createDirectory, createFile, debugFS, directoryContent,
                                        fileContent, findFile, goToDirectory, information,
                                        removeFileOrDirectory, writeToFile)
import FileManager.FileSystemTypes
import FileManager.VCSHandlers (addToVCS, allHistoryVCS, fileHistoryVCS, fileVersionVCS, initVCS,
                                mergeFileRevsVCS, removeFileRevFromVCS, removeFromVCS, updateInVCS)
import Loaders.Downloader (getFileSystem)
import Loaders.Uploader (uploadFileSystem)
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)
import Utils.Parser (splitArguments)

type ErrorMessage = String

data Opts = Opts { optCommand :: !Command }

data Command
  = Dir
  | Exit
  | Cd FilePath
  | Ls FilePath
  | Cat String
  | Remove FilePath
  | FindFile String
  | CreateFile String
  | CreateFolder String
  | Information FilePath
  | WriteToFile FilePath String
  | VCSInit
  | VCSAdd FilePath
  | VCSUpdate FilePath String
  | VCSHistory FilePath
  | VCSCat FilePath Integer
  | VCSShowAll
  | VCSRemove FilePath
  | VCSRemoveRev FilePath Integer
  | VCSMergeRevs FilePath Integer Integer String
  | Debug

main :: IO ()
main = do
  a <- (\x -> if x == [] then "" else head x) <$> getArgs
  -- maybe error here
  fs <- makeAbsolute a >>= \curDir -> getFileSystem curDir
  let initState = FSState fs "" Nothing
  runInteractive initState
  return ()

getArguments :: FSState -> IO [String]
getArguments st = do
  printPrompt st
  s <- getLine
  case splitArguments s of
    Nothing  -> do
      putStrLn $ "Oops! Mismatched quotes"
      getArguments st
    (Just a) -> return a

runInteractive :: FSState -> IO ()
runInteractive st@FSState{curFileSystem = fs} = do
  args <- getArguments st
  let parsRes = execParserPure defaultPrefs optsParser args
  unpackedRes <- customHandleParserResult parsRes
  time <- getCurrentTime
  case unpackedRes of
    Left msg -> do
      putStrLn msg
      runInteractive st
    Right opts ->
      case optCommand opts of
        Debug                   -> callHandlerString debugFS ""
        Dir                     -> callHandlerString directoryContent ""
        Exit                    -> uploadFileSystem fs >> putStrLn "Bye-bye"
        Cd path                 -> callHandlerVoid  goToDirectory path
        Ls path                 -> callHandlerString directoryContent path
        Cat path                -> callHandlerByteString fileContent path
        Remove path             -> callHandlerVoid removeFileOrDirectory path
        FindFile name           -> callHandlerString findFile name
        CreateFile name         -> callHandlerVoid createFile (name, time)
        Information path        -> callHandlerString information path
        CreateFolder name       -> callHandlerVoid createDirectory name
        WriteToFile path cont   -> callHandlerVoid writeToFile ((B.fromString cont), path, time)
        VCSInit                 -> callHandlerString0 initVCS
        VCSAdd path             -> callHandlerString addToVCS path
        VCSUpdate path msg      -> callHandlerString updateInVCS (path, msg)
        VCSHistory path         -> callHandlerString fileHistoryVCS path
        VCSCat path i           -> callHandlerByteString fileVersionVCS (path, i)
        VCSRemove path          -> callHandlerString removeFromVCS path
        VCSRemoveRev path i     -> callHandlerString removeFileRevFromVCS (path, i)
        VCSMergeRevs path i j s -> callHandlerString mergeFileRevsVCS (path, i, j, s, time)
        VCSShowAll              -> callHandlerString0 allHistoryVCS
  where
    callHandlerVoid foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ "Oops: " ++ (show err)
        (Right _ ) -> return ()
      runInteractive newState
    callHandlerString0 foo = do
      let (res, newState) = runState (runExceptT foo) st
      case res of
        (Left err) -> putStrLn $ "Oops: " ++ (show err)
        (Right s ) -> putStrLn s
      runInteractive newState
    callHandlerString foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ "Oops: " ++ (show err)
        (Right s ) -> putStrLn s
      runInteractive newState
    callHandlerByteString foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ "Oops: " ++ (show err)
        (Right s ) -> BS.putStrLn s
      runInteractive newState

printPrompt :: FSState -> IO ()
printPrompt FSState{curFileSystem = fs, curDirectoryPath = path} = do
  putStr $ "[" ++ (getPathToRootDirectory fs) ++ "/" ++ path ++ "]: "
  hFlush stdout

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (  fullDesc
    <> progDesc "Watcher: FileManager & Control System"
    <> header "This is header"
    )

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1" (short 'v' <> long "version" <> help "show version")

programOptions :: Parser Opts
programOptions =
  Opts <$> hsubparser
    (  dirCommand
    <> exitCommand
    <> cdCommand
    <> lsCommand
    <> catCommand
    <> removeCommand
    <> findFileCommand
    <> createFileCommand
    <> createFolderCommand
    <> informationCommand
    <> writeToFileCommand
    <> vcsInitCommand
    <> vcsAddCommand
    <> vcsUpdateCommand
    <> vcsHistoryCommand
    <> vcsCatCommand
    <> vcsRemoveCommand
    <> vcsRemoveRevCommand
    <> vcsMergeRevsCommand
    <> vcsShowAllCommand
    <> debugCommand
    )
  where
    dirCommand :: Mod CommandFields Command
    dirCommand = command
      "dir"
      (info (pure Dir) (progDesc "show content of current folder"))
    exitCommand :: Mod CommandFields Command
    exitCommand = command
      "exit"
      (info (pure Exit) (progDesc "exit from Watcher and save changes"))
    cdCommand :: Mod CommandFields Command
    cdCommand = command
      "cd"
      (info cdOptions (progDesc "go to specified folder"))
    lsCommand :: Mod CommandFields Command
    lsCommand = command
      "ls"
      (info lsOptions (progDesc "show content of specified folder"))
    catCommand :: Mod CommandFields Command
    catCommand = command
      "cat"
      (info catOptions (progDesc "show content of specified file"))
    removeCommand :: Mod CommandFields Command
    removeCommand = command
      "remove"
      (info removeOptions (progDesc "remove specified file/directory"))
    findFileCommand :: Mod CommandFields Command
    findFileCommand = command
      "find-file"
      (info findFileOptions (progDesc "find files with specified name"))
    createFileCommand :: Mod CommandFields Command
    createFileCommand = command
      "create-file"
      (info createFileOptions (progDesc "create file in current folder with specified name"))
    createFolderCommand :: Mod CommandFields Command
    createFolderCommand = command
      "create-folder"
      (info createFolderOptions (progDesc "create folder in current folder with specified name"))
    informationCommand :: Mod CommandFields Command
    informationCommand = command
      "information"
      (info informationOptions (progDesc "show information for specified folder/file"))
    writeToFileCommand :: Mod CommandFields Command
    writeToFileCommand = command
      "write-file"
      (info writeToFileOptions (progDesc "write text into file"))
    vcsInitCommand :: Mod CommandFields Command
    vcsInitCommand = command
      "vcs-init"
      (info (pure VCSInit) (progDesc "init VCS in current directory"))
    vcsAddCommand :: Mod CommandFields Command
    vcsAddCommand = command
      "vcs-add"
      (info vcsAddOptions (progDesc "add specified file/directory to VCS"))
    vcsUpdateCommand :: Mod CommandFields Command
    vcsUpdateCommand = command
      "vcs-update"
      (info vcsUpdateOptions (progDesc "update specified file in VCS"))
    vcsHistoryCommand :: Mod CommandFields Command
    vcsHistoryCommand = command
      "vcs-history"
      (info vcsHistoryOptions (progDesc "update specified file in VCS"))
    vcsCatCommand :: Mod CommandFields Command
    vcsCatCommand = command
      "vcs-cat"
      (info vcsCatOptions (progDesc "show specified version of specified file in VCS"))
    vcsRemoveCommand :: Mod CommandFields Command
    vcsRemoveCommand = command
      "vcs-remove"
      (info vcsRemoveOptions (progDesc "remove specified file from VCS"))
    vcsRemoveRevCommand :: Mod CommandFields Command
    vcsRemoveRevCommand = command
      "vcs-remove-rev"
      (info vcsRemoveRevOptions (progDesc "remove specified revision of specified file from VCS"))
    vcsMergeRevsCommand :: Mod CommandFields Command
    vcsMergeRevsCommand = command
      "vcs-merge-revs"
      (info vcsMergeRevsOptions (progDesc "remove specified revision of specified file from VCS"))
    vcsShowAllCommand :: Mod CommandFields Command
    vcsShowAllCommand = command
      "vcs-show-all"
      (info (pure VCSShowAll) (progDesc "show all VCS history"))
    cdOptions :: Parser Command
    cdOptions = Cd <$>
      strArgument (metavar "PATH" <> help "Path to folder where to go")
    lsOptions :: Parser Command
    lsOptions = Ls <$>
      strArgument (metavar "PATH" <> help "Path to folder to show contents for")
    catOptions :: Parser Command
    catOptions = Cat <$>
      strArgument (metavar "PATH" <> help "Path to file to show contents for")
    removeOptions = Remove <$>
      strArgument (metavar "PATH" <> help "Path to file/directory to be deleted")
    findFileOptions :: Parser Command
    findFileOptions = FindFile <$>
      strArgument (metavar "NAME" <> help "Name of file to be found")
    createFileOptions :: Parser Command
    createFileOptions = CreateFile <$>
      strArgument (metavar "NAME" <> help "Name of file to be created")
    createFolderOptions :: Parser Command
    createFolderOptions = CreateFolder <$>
      strArgument (metavar "NAME" <> help "Name of folder to be created")
    informationOptions :: Parser Command
    informationOptions = Information <$>
      strArgument (metavar "PATH" <> help "Path to file/folder to show information for")
    writeToFileOptions :: Parser Command
    writeToFileOptions = WriteToFile <$>
      strArgument (metavar "PATH" <> help "Path to file to write text in") <*>
      strArgument (metavar "TEXT" <> help "Text to write in file")
    vcsAddOptions :: Parser Command
    vcsAddOptions = VCSAdd <$>
      strArgument (metavar "PATH" <> help "Path to file/directory to add into current VCS")
    vcsUpdateOptions :: Parser Command
    vcsUpdateOptions = VCSUpdate <$>
      strArgument (metavar "PATH" <> help "Path to file to updaten in current VCS") <*>
      strArgument (metavar "MESSAGE" <> help "Message to show for this update")
    vcsHistoryOptions :: Parser Command
    vcsHistoryOptions = VCSHistory <$>
      strArgument (metavar "PATH" <> help "Path to file/directory that is in VCS")
    vcsCatOptions :: Parser Command
    vcsCatOptions = VCSCat <$>
      strArgument (metavar "PATH" <> help "Path to file that is in VCS") <*>
      argument auto (metavar "INDEX" <> help "Index of file in vcs")
    vcsRemoveOptions :: Parser Command
    vcsRemoveOptions = VCSRemove <$>
      strArgument (metavar "PATH" <> help "Path to file to be deleted from VCS")
    vcsRemoveRevOptions :: Parser Command
    vcsRemoveRevOptions = VCSRemoveRev <$>
      strArgument (metavar "PATH" <> help "Path to file that is in VCS") <*>
      argument auto (metavar "INDEX" <> help "Index of file in vcs")
    vcsMergeRevsOptions :: Parser Command
    vcsMergeRevsOptions = VCSMergeRevs <$>
      strArgument (metavar "PATH" <> help "Path to file that is in VCS") <*>
      argument auto (metavar "INDEX" <> help "Index of file in vcs") <*>
      argument auto (metavar "INDEX" <> help "Index of file in vcs") <*>
      strArgument (metavar "STRATEGY" <> help "strategy of merging revisions VCS")
    debugCommand :: Mod CommandFields Command
    debugCommand = command
      "debug"
      (info (pure Debug) (progDesc "debug"))

customHandleParserResult :: ParserResult a -> IO (Either ErrorMessage a)
customHandleParserResult (Success a) = return $ Right a
customHandleParserResult (Failure f) = do
  progn <- getProgName
  let (msg, _) = renderFailure f progn
  return $ Left msg
customHandleParserResult (CompletionInvoked compl) = do
  progn <- getProgName
  msg <- execCompletion compl progn
  return $ Left msg
