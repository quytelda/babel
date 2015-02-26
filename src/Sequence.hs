--
-- Sequence.hs -- sequence generation functions
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

module Sequence
       ( parsePattern
       , generateDefMap
       , generateSequence
       ) where


import System.Random

import Data.Text(pack, unpack, splitOn, strip)
import qualified Data.Map as Map

parsePattern :: String -> [String]
-- ^Attempt to parse a String into a list of pattern keys.
parsePattern str = Prelude.map unpack (splitOn (pack ":") (pack str))


generateDefMap :: [String] -> Map.Map String [String]
-- ^Generate a map from a list of strings representing the pattern definitions.
generateDefMap contents =
  let definitions = map parseDef contents
      in Map.fromList definitions


parseDef :: String -> (String, [String])
-- ^Parse a definition from a string of the form "KEY = VALUE"
parseDef line =
  let def = filter ( /= ' ') line
      (key, value) = break ( == '=') def
      in (key, subdivide $ tail value)


generateSequence :: [String] -> Map.Map String [String] -> IO [String]
-- ^Generate a randomized sequence based on the provided pattern and pattern definition map.
generateSequence pattern defMap =
  mapM (\key -> replace key defMap) pattern


selectRandom :: [a] -> IO a
-- ^Select a random item from a list
selectRandom list = randomRIO (0, length list - 1) >>=
                    (\x -> return (list !! x))

subdivide :: [a] -> [[a]]
-- ^Subdivides a list into a list of lists for each element of the original.
subdivide [] = []
subdivide (x:xs) = [x] : subdivide xs

replace :: String -> Map.Map String [String] -> IO String
replace [] _ = return ""
replace key defMap = if (head k == '(') && (last k == ')') then do
                       chance <- (randomIO :: IO Bool)

                       if chance then replace ((tail . init) k) defMap
                                      else return ""
                     else
                       case Map.lookup key defMap of
                         (Just values) -> selectRandom values
                         (Nothing) -> return ""
  where
    trim = unpack . strip . pack
    k = trim key
