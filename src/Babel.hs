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
    help
    exitFailure

  -- parse pattern
  let pattern = splitBy ':' (args !! 0)
  m <- loadDefs "babel.conf"

  results <- (replicateM 5 (generate pattern m))
  mapM putStrLn (map concat results)

  return ()


help :: IO ()
help = do
  putStrLn "Usage: babel [options] <pattern>"
  putStrLn "\nOptions:"
  putStrLn "-h\t--help\tShow this information"
  putStrLn "-n <N>\t\tGenerate <N> sequences."
  putStrLn "-c\t--config <path>\tUse config file at <path>."


loadDefs :: FilePath -> IO (Map.Map String String)
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
splitBy _ [] = []
splitBy del xs = front : splitBy del (tail' back)
  where
    (front, back) = break ( == del) xs
    tail' [] = []
    tail' x = tail x


generate :: [String] -> Map.Map String String -> IO [String]
generate pattern m = mapM (\k -> substitute k m) pattern


substitute :: String -> Map.Map String String -> IO String
substitute k m =
  case (Map.lookup k m) of
  (Just value) -> do
    elem <- selectRandom value
    return [elem]
  (Nothing) -> do
    return ""


selectRandom :: [a] -> IO a
selectRandom list = do
  i <- randomRIO (0, max)
  return (list !! i)
  where max = (length list) - 1
