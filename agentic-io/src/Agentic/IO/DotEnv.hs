-- | Loading provider keys from a @.env@ file.
module Agentic.IO.DotEnv
  ( loadDotEnv
  , parseDotEnv
  ) where

import Control.Monad (filterM, forM_, when)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Maybe (isNothing, listToMaybe)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath (takeDirectory, (</>))

-- | Find the nearest @.env@, from the current directory upwards, and set each
-- variable it defines that isn't already set. Returns the file it loaded.
loadDotEnv :: IO (Maybe FilePath)
loadDotEnv = do
  here <- getCurrentDirectory
  found <- listToMaybe <$> filterM doesFileExist (map (</> ".env") (upwards here))
  forM_ found $ \file -> do
    vars <- parseDotEnv <$> readFile file
    forM_ vars $ \(key, value) -> do
      existing <- lookupEnv key
      when (isNothing existing && not (null value)) (setEnv key value)
  pure found
  where
    upwards dir
      | takeDirectory dir == dir = [dir]
      | otherwise = dir : upwards (takeDirectory dir)

-- | @KEY=value@ lines. Blank lines and @#@ comments are skipped, an @export@
-- prefix is allowed, and matching quotes around a value are removed.
parseDotEnv :: String -> [(String, String)]
parseDotEnv = concatMap entry . lines
  where
    entry raw = case break (== '=') (strip (dropExport (strip raw))) of
      (key, '=' : value) | not (null key), not ("#" `isPrefixOf` key) -> [(strip key, unquote (strip value))]
      _ -> []
    dropExport l = if "export " `isPrefixOf` l then drop 7 l else l
    strip = dropWhileEnd isSpace . dropWhile isSpace
    unquote v = case v of
      q : rest | q `elem` ("\"'" :: String), not (null rest), last rest == q -> init rest
      _ -> v
