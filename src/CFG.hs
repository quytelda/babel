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

data Symbol = Node String
            | Terminal String
            deriving (Eq, Show, Ord)

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
expand cfg sym@(Node nt) = do
  symbols <- produce cfg sym
  concat <$> mapM (expand cfg) symbols
expand cfg sym@(Terminal t) = return t

-- Parsec parser for CFGs

whitespace = many (oneOf " \t")

cfg = endBy rules (char '\n')

rules = do
  _ <- whitespace
  var <- variable
  _ <- whitespace
  opt <- options
  return (var, opt)

variable = do
  _ <- whitespace
  result <- symbol
  _ <- whitespace
  string "->"
  _ <- whitespace
  return result

options = do
  _ <- whitespace
  sepBy symbol (char '|')
symbol = do
  _ <- whitespace
  result <- many (noneOf "|->\n ")
  _ <- whitespace
  return result

parseCFG :: String -> FilePath -> Either ParseError [(String, [String])]
parseCFG input fp = parse cfg fp input

{-| Randomly select an element from a list. -}
pickRIO :: [a] -> IO (Maybe a)
pickRIO [] = return Nothing
pickRIO xs = randomRIO (0, length xs - 1)
             >>= return . Just . (xs !!)
