module FileManager.FilePathUtils
  ( getNormalisedPath
  , getNormalisedSplittedPath
  , getFSPathForDirectory
  ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (intercalate, unfoldr)
import FileManager.FileSystemTypes
import System.FilePath (isPathSeparator, pathSeparator)
import System.FilePath.Posix (dropTrailingPathSeparator, joinPath, makeRelative, normalise,
                              splitDirectories)

getFSPathForDirectory :: Directory -> ExceptT FSException (State FSState) FilePath
getFSPathForDirectory dir = do
  FSState{curFileSystem = fs} <- get
  return $ dotToEmpty $ makeRelative (getPathToRootDirectory fs) (getDirPath $ getDirInfo dir)

getNormalisedPath :: FilePath -> ExceptT FSException (State FSState) FilePath
getNormalisedPath path = do
  splittedPath <- getNormalisedSplittedPath path
  return $ joinPath splittedPath

getNormalisedSplittedPath :: FilePath -> ExceptT FSException (State FSState) [FilePath]
getNormalisedSplittedPath path = do
  case customNormalise path of
    Nothing   -> throwE $ NotValidPath path
    (Just np) -> return $ getSplittedPath $ np

customNormalise :: FilePath -> Maybe FilePath
customNormalise path =
  dotToEmpty <$> (guess_dotdot . (dropTrailingPathSeparator . normalise)) path

dotToEmpty :: FilePath -> FilePath
dotToEmpty = \x -> if x == "." then "" else x

getSplittedPath :: FilePath -> [FilePath]
getSplittedPath path = do
  let splittedPath = splitDirectories $ dropTrailingPathSeparator path
  case splittedPath of
    ("/" : xs) -> xs
    _          -> splittedPath

-- Code below is a copy-paste from System.Path.NameManip

guess_dotdot :: String -> Maybe String
guess_dotdot = fmap unslice_path . guess_dotdot_comps . slice_path

slice_path :: String -> [String]
slice_path "" = []
slice_path (c:cs) = if isPathSeparator c
  then case slice_path' cs of
    []     -> [[c]]
    (p:ps) -> (c:p):ps
  else slice_path' (c:cs)
  where
    slice_path' o = filter (\cur -> cur /= "" && cur /= ".") (split o)
    split xs = unfoldr f xs
      where
        f "" = Nothing
        f ys = Just $ fmap tail' $ break isPathSeparator ys
        tail' [] = []
        tail' zs = tail zs

unslice_path :: [String]  -> String
unslice_path [] = "."
unslice_path cs = intercalate [pathSeparator] cs

guess_dotdot_comps :: [String] -> Maybe [String]
guess_dotdot_comps = guess_dotdot_comps' []
   where
      guess_dotdot_comps' schon [] = Just schon
      guess_dotdot_comps' [] ("..":_) = Nothing
      guess_dotdot_comps' schon ("..":teile) =
        guess_dotdot_comps' (reverse . tail . reverse $ schon) teile
      guess_dotdot_comps' schon (teil:teile) =
        guess_dotdot_comps' (schon ++ [teil]) teile
