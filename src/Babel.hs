--
-- Babel.hs -- configurable sequence generator
-- Copyright (C) 2015 Quytelda <quytelda@tamalin.org>
--
-- This file is part of babel.

-- babel is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- babel is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with babel.  If not, see <http://www.gnu.org/licenses/>.
--

module Main where

import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.Random
import System.Directory

import qualified Data.Map as Map
import Control.Monad(when, mapM, replicateM)

default_defs = Map.fromList[("UC", ['A'..'Z']), ("LC", ['a'..'z']), ("D", ['0'..'9'])]

main :: IO ()
main = do

  -- argument handling
  args <- getArgs
  when (length args < 1) $ do
    hPutStrLn stderr "Not enough arguments."
    usage
    exitFailure

  -- parse pattern
  let pattern = splitBy ':' (args !! 0)
  m <- loadDefs "babel.defs"

  results <- (replicateM 5 (generate pattern m))
  mapM putStrLn (map concat results)

  return ()


usage :: IO ()
usage = putStrLn "Usage: babel [options] <pattern>"


loadDefs :: FilePath -> IO (Map.Map String String)
-- ^Load custom sequence pattern definitions from a file.
loadDefs path = do
  exists <- doesFileExist path

  if exists then do
    contents <- openFile path ReadMode >>= hGetContents
    return $ Map.fromList $
      map (trimSnd.break ( == '=')) (lines contents)
    else
    return default_defs
  where
    trimSnd (first, second) = (first, tail second)


splitBy :: Char -> String -> [String]
-- ^Use a character delimiter to split a String into smaller components
splitBy _ [] = []
splitBy del xs = front : splitBy del (tail' back)
  where
    (front, back) = break ( == del) xs
    tail' [] = []
    tail' x = tail x


generate :: [String] -> Map.Map String String -> IO [String]
-- ^use the provided pattern to randomly generate a matching sequence.
generate pattern m = mapM (\k -> substitute k m) pattern


substitute :: String -> Map.Map String String -> IO String
-- ^Look up a random substitute for the provided key in the key/value map.
substitute k m
  | (head k == '(' && last k == ')') = do
      include <- chance

      if include then
        substitute (tail (init k)) m
      else
        return ""
  | otherwise =
      case (Map.lookup k m) of
       (Just value) -> do
         elem <- selectRandom value
         return [elem]
       (Nothing) -> do
         return ""
  where
    chance = randomIO :: IO Bool


selectRandom :: [a] -> IO a
-- ^Pick a random item from a list
selectRandom list = do
  i <- randomRIO (0, max)
  return (list !! i)
  where max = (length list) - 1
