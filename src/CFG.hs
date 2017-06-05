-- Main.hs -- The entry point for the babel sequence generator
-- Copyright (C) 2016 Quytelda Kahja <quytelda@tamalin.org>
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

module CFG (produce, expand, parseCFG) where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Map
import System.Random
import Text.ParserCombinators.Parsec

type Symbol = String
type CFG = (Map Symbol [[Symbol]])

{-| Follow a single production, randomly selecting between alternative rules. -}
produce :: CFG -> Symbol -> IO [Symbol]
produce cfg sym =
  case lookup sym cfg of
    Just [] -> return []
    Just ss -> fromJust <$> (pickRIO ss) >>= return
    _       -> return []

{-| Use a series of random productions to expand a CFG grammer into a set of
terminal symbols -}
expand :: CFG -> Symbol -> IO String
expand cfg sym =
  case lookup sym cfg of
    Just nt -> do
      symbols <- produce cfg sym
      concat <$> mapM (expand cfg) symbols
    Nothing -> return sym

parseCFG :: String -> Either ParseError CFG
parseCFG input = undefined

{-| Randomly select an element from a list. -}
pickRIO :: [a] -> IO (Maybe a)
pickRIO [] = return Nothing
pickRIO xs = randomRIO (0, length xs - 1)
             >>= return . Just . (xs !!)
