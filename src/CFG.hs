-- CFG.hs -- Context Free Grammar representation, parsing, and application.
-- Copyright (C) 2017 Quytelda Kahja <quytelda@tamalin.org>
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

module CFG (produce, expand, parseCFG, Symbol) where

import Data.Functor
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.Random
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

type Symbol = String
type CFG = (Map.Map Symbol [[Symbol]])

{-| Follow a single production, randomly selecting between alternative rules. -}
produce :: CFG -> Symbol -> IO [Symbol]
produce cfg sym =
  case Map.lookup sym cfg of
    Just [] -> return []
    Just ss -> fromJust <$> (pickRIO ss) >>= return
    _       -> return []

{-| Use a series of random productions to expand a CFG grammer into a set of
terminal symbols -}
expand :: CFG -> Symbol -> IO String
expand cfg sym =
  case Map.lookup sym cfg of
    Just _ -> do
      symbols <- produce cfg sym
      concat <$> mapM (expand cfg) symbols
    Nothing -> return sym

{-| Randomly select an element from a list. -}
pickRIO :: [a] -> IO (Maybe a)
pickRIO [] = return Nothing
pickRIO xs = randomRIO (0, length xs - 1)
             >>= return . Just . (xs !!)

{-| Parse a string description of a context free grammer into a CFG type. -}
parseCFG :: String -> Either ParseError CFG
parseCFG input = parse cfgParse "" input

{-| lexeme parses something with the parser p, ignoring leading and trailing
whitespace. -}
lexeme :: Parser a -> Parser a
lexeme p = do
  _ <- skipMany (oneOf " \t")
  result <- p
  _ <- skipMany (oneOf " \t")
  return result

{-| symbol parses a string of characters until it reaches whitespace or a
special control character. -}
symbol :: Parser Symbol
symbol = many1 (noneOf "|-> \n")

{-| groups parses a series of whitespace seperated symbols. -}
groups :: Parser [Symbol]
groups = lexeme $ sepEndBy1 symbol (char ' ')

{-| alternates parses a list of alternative productions for a non-terminal in
the CFG grammar. -}
alternates :: Parser [[Symbol]]
alternates = sepBy1 groups (char '|')

{-| rule parses a single CFG rule, which maps a variable to it possible
derivations when it is produced. -}
rule :: Parser (Symbol, [[Symbol]])
rule = do
  var <- symbol
  _ <- lexeme $ string "->"
  alt <- alternates
  return (var, alt)

{-| cfg parses a string representing a context free grammar in the format of
line by line rules mapping variables to productions. It returns a dictionary of
these mappings. -}
cfgParse :: Parser CFG
cfgParse = do
  rules <- sepEndBy (lexeme rule) endOfLine
  return $ Map.fromList (compress rules [])
  where compress [] a = a
        compress (r@(var, ss) : rs) a =
          case lookup var a of
            (Just ts) ->
              let r' = (var, ss ++ ts)
                  a' = filter (\x -> var /= fst x) a
              in compress rs (r' : a')
            _         -> compress rs (r : a)


