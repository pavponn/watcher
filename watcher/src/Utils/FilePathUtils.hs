module Utils.FilePathUtils
  ( getNormalisedPath
  , getNormalisedSplittedPath
  , getFSPathForDirectory
  ) where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.List (intercalate, unfoldr)
import FileSystemTypes
import System.FilePath (isPathSeparator, pathSeparator)
import System.FilePath.Posix (dropTrailingPathSeparator, joinPath, makeRelative, normalise,
                              splitDirectories)

-- | Accepts `Directory` and returns path to it from root directory of `FileSystem`.
getFSPathForDirectory :: Directory -> ExceptState FilePath
getFSPathForDirectory dir = do
  FSState{curFileSystem = fs} <- get
  return $ dotToEmpty $ makeRelative (getPathToRootDirectory fs) (getDirPath $ getDirInfo dir)

-- | Same as `getNormalisedSplittedPath`, but joins a path.
getNormalisedPath :: FilePath -> ExceptState FilePath
getNormalisedPath path = do
  splittedPath <- getNormalisedSplittedPath path
  return $ joinPath splittedPath

-- Returns path as list of directories. Path is already normalised and doesn't
-- contain ".", ".." or "/". Throws NotValidPath if `customNormalise` returns Nothing for this path.
-- May happen when moves like "../../src/../" can't be derived, due to current FSState.
-- Accepts absolute paths (absolute regarding root directory in `FileSystem`) or
-- relative path to root. TODO: fix logic of path's work.
getNormalisedSplittedPath :: FilePath -> ExceptState [FilePath]
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
