{-#LANGUAGE ScopedTypeVariables#-}

module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Semigroup ((<>))
import FileManager.FileSystemTypes
import FileManager.Handlers (FSState (..), directoryContent, goToDirectory)
import FileManager.Loader (getFileSystem)
import Options.Applicative
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data Opts = Opts {optCommand :: !Command }

data Command
  = Dir
  | Exit
  | Cd FilePath
  | Ls FilePath
  | Cat String
  | FindFile String
  | CreateFile String
  | CreateFolder String
  | Information FilePath

main :: IO ()
main = do
  a <- (\x -> if x == [] then "" else head x) <$> getArgs
  fs <- makeAbsolute a >>= \curDir -> getFileSystem curDir
  putStrLn $ show fs
  let initState = FSState{fileSystem = fs, curDirectoryPath = ""}
  runInteractive initState
  return ()

runInteractive :: FSState -> IO ()
runInteractive st = do
  printPrompt st
  args <- words <$> getLine
  let parsRes = execParserPure defaultPrefs optsParser args
  case parsRes of
    Failure failure -> do
      putStrLn "fail"
      runInteractive st
    Success opts ->
      case optCommand opts of
        Dir -> do
          let (res, newState) = runState (runExceptT $ directoryContent "") st
          case res of
            (Left err)   -> putStrLn $ show err
            (Right cont) -> putStrLn cont
          runInteractive newState
        (Cd path) -> do
          let (res, newState) = runState (runExceptT $ goToDirectory path) st
          case res of
            (Left err)   -> putStrLn $ show err
            (Right cont) -> putStrLn "completed"
          runInteractive newState
        Exit  -> putStrLn "exit"
        _     -> putStrLn "other cmd"

printPrompt :: FSState -> IO ()
printPrompt FSState{fileSystem = fs, curDirectoryPath = path} = do
  putStr $ (getPathToRootDirectory fs) ++ path ++ ">>> "
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
    <> findFileCommand
    <> createFileCommand
    <> createFolderCommand
    <> informationCommand
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
    cdOptions :: Parser Command
    cdOptions = Cd <$>
      strArgument (metavar "PATH" <> help "Path to folder where to go")
    lsOptions :: Parser Command
    lsOptions = Ls <$>
      strArgument (metavar "PATH" <> help "Path to folder to show contents for")
    catOptions :: Parser Command
    catOptions = Cat <$>
      strArgument (metavar "PATH" <> help "Path to file to show contents for")
    findFileOptions :: Parser Command
    findFileOptions = FindFile <$>
      strArgument (metavar "NAME" <> help "Name of file to be found")
    createFileOptions :: Parser Command
    createFileOptions = CreateFile <$>
      strArgument (metavar "NAME" <> help "Name of file to be created")
    createFolderOptions :: Parser Command
    createFolderOptions = CreateFile <$>
      strArgument (metavar "NAME" <> help "Name of folder to be created")
    informationOptions :: Parser Command
    informationOptions = Information <$>
      strArgument (metavar "PATH" <> help "Path to file/folder to show information for")
