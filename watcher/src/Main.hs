{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.ByteString.Char8 as B
import Data.Semigroup ((<>))
import FileManager.FileSystemTypes
import FileManager.Handlers (createDirectory, createFile, debugFS, directoryContent,
                             fileContent, findFile, goToDirectory, information,
                             removeFileOrDirectory, writeToFile)
import FileManager.Loader (getFileSystem)
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)

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
  | Debug

main :: IO ()
main = do
  a <- (\x -> if x == [] then "" else head x) <$> getArgs
  fs <- makeAbsolute a >>= \curDir -> getFileSystem curDir
  let initState = FSState{fileSystem = fs, curDirectoryPath = ""}
  runInteractive initState
  return ()

runInteractive :: FSState -> IO ()
runInteractive st = do
  printPrompt st
  args <- words <$> getLine
  let parsRes = execParserPure defaultPrefs optsParser args
  unpackedRes <- customHandleParserResult parsRes
  case unpackedRes of
    Left msg -> do
      putStrLn msg
      runInteractive st
    Right opts ->
      case optCommand opts of
        Debug                   -> handleOperationString debugFS ""
        Dir                     -> handleOperationString directoryContent ""
        Exit                    -> putStrLn "Bye-bye"
        (Cd path)               -> handleOperationVoid  goToDirectory path
        (Ls path)               -> handleOperationString directoryContent path
        (Cat path)              -> handleOperationByteString fileContent path
        (Remove path)           -> handleOperationVoid removeFileOrDirectory path
        (FindFile name)         -> handleOperationString findFile name
        (CreateFile name)       -> handleOperationVoid createFile name
        (Information path)      -> handleOperationString information path
        (CreateFolder name)     -> handleOperationVoid createDirectory name
        (WriteToFile path cont) -> handleOperationVoid2 writeToFile (B.pack cont) path
  where
    handleOperationVoid2 foo arg1 arg2 = do
      let (res, newState) = runState (runExceptT $ foo arg1 arg2) st
      case res of
        (Left err) -> putStrLn $ show err
        (Right _ ) -> return ()
      runInteractive newState
    handleOperationVoid foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ show err
        (Right _ ) -> return ()
      runInteractive newState
    handleOperationString foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ show err
        (Right s ) -> putStrLn s
      runInteractive newState
    handleOperationByteString foo arg = do
      let (res, newState) = runState (runExceptT $ foo arg) st
      case res of
        (Left err) -> putStrLn $ show err
        (Right s ) -> B.putStrLn s
      runInteractive newState

printPrompt :: FSState -> IO ()
printPrompt FSState{fileSystem = fs, curDirectoryPath = path} = do
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
versionOption = infoOption "0.1" (long "version" <> help "Show version")

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
