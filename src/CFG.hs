{-# LANGUAGE RecordWildCards #-}
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

import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import System.Random
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import Random

type Symbol = String
data CFG = CFG { crypto :: Bool
               , cfgMap :: (Map.Map Symbol [[Symbol]])
               }

instance Show CFG where
  {-| Convert a CFG record into a String.  Each line will show an individual rule
  in the format SYMBOL -> A B C | D E F | ... -}
  show CFG{..} = unlines $ map showRule (Map.toList cfgMap)
    where showRule (sym, subs) = "'" ++ sym ++ " -> " ++ (showSubs subs) ++ "'"
          showSubs subs = concat $ (intersperse " | ") (joinAlts subs)
          joinAlts subs = map (concat . intersperse " ") subs

{-| Use a series of random productions to expand a CFG grammer into a set of
terminal symbols -}
expand :: CFG -> Symbol -> IO String
expand cfg@CFG{..} sym =
  case Map.lookup sym cfgMap of
    Just _ -> do
      symbols <- produce cfg sym
      concat <$> mapM (expand cfg) symbols
    Nothing -> return sym

{-| Follow a single production, randomly selecting between alternative rules. -}
produce :: CFG -> Symbol -> IO [Symbol]
produce CFG{..} sym =
  case Map.lookup sym cfgMap of
    Just [] -> return []
    Just ss -> fromJust <$> (pickRIO chooser ss) >>= return
    _       -> return []
  where chooser = if crypto
                  then randomCryptIO
                  else randomRIO

{-| Removes syntactically null content from a CFG definition file, such as
comments, empty lines, and blank lines. -}
stripNulls :: String -> String
stripNulls text = let origLines  = lines text
                      tmpLines   = map dropComment origLines
                      cleanLines = filter (not . isBlankLine) tmpLines
                  in unlines cleanLines
  where dropComment = takeWhile (/= '#')
        isBlankLine = all isSpace

{-| Parse a string description of a context free grammer into a CFG type. -}
parseCFG :: Bool -> String -> Either ParseError CFG
parseCFG crypto input = parse (cfgParse crypto) "" (stripNulls input)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (oneOf " \t")

{-| lexeme parses something with the parser p, ignoring leading and trailing
whitespace. -}
lexeme :: Parser a -> Parser a
lexeme p = do
  skipWhitespace
  result <- p
  skipWhitespace
  return result

{-| symbol parses a string of characters until it reaches whitespace or a
special control character. -}
symbol :: Parser Symbol
symbol = many1 (noneOf "|-> \t\f\r\n")

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

{-| cfgParse parses a string representing a context free grammar in the format
of line by line rules mapping variables to productions. It returns a dictionary
of these mappings. -}
cfgParse :: Bool -> Parser CFG
cfgParse crypto = do
  rules <- sepEndBy (lexeme rule) endOfLine

  let cfgMap = Map.fromList (compress rules [])
  return $ CFG crypto cfgMap

  where compress [] a = a
        compress (r@(var, ss) : rs) a =
          case lookup var a of
            (Just ts) ->
              let r' = (var, ss ++ ts)
                  a' = filter (\x -> var /= fst x) a
              in compress rs (r' : a')
            _         -> compress rs (r : a)
