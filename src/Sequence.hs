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

module Sequence where


import System.Random
import Data.Map as Map

parsePattern :: String -> Maybe [String]
-- ^Attempt to parse a String into a list of pattern keys.
parsePattern str = undefined


generateSequence :: [String] -> Map.Map String [String] -> IO [String]
-- ^Generate a randomized sequence based on the provided pattern and pattern definition map.
generateSequence pattern defMap = undefined


selectRandom :: [a] -> IO a
-- ^Select a random item from a list
selectRandom list = randomRIO (0, length list - 1) >>=
                    (\x -> return (list !! x))
