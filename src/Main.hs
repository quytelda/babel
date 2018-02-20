-- Main.hs -- The entry point for the babel sequence generator
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

module Main where

import Control.Monad(when, replicateM)
import Data.List.Split
import Data.Version
import System.Console.GetOpt
import System.Environment
import System.Exit (exitSuccess, exitFailure)
import System.IO

import CFG

releaseVersion :: Version
releaseVersion = Version [2, 1, 0] []

-- | The Options record holds a representation of the runtime configuration.
data Options = Options { optHelp     :: Bool
                       , optNumber   :: Int
                       , optOutput   :: (String -> IO ())
                       , optTemplate :: [Symbol]
                       , optVersion  :: Bool
                       }

-- | @defaults@ is the default runtime configuration record.
defaults :: Options
defaults = Options { optHelp     = False
                   , optNumber   = 1
                   , optOutput   = putStrLn
                   , optTemplate = ["S"]
                   , optVersion  = False
                   }

-- | @options@ describes the supported command-line arguments
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opt -> opt {optHelp = True}))
    "Display help and usage information."
  , Option ['n'] []
    (ReqArg (\str opt -> opt {optNumber = (read str :: Int)}) "N")
    "Generate N different sequences."
  , Option ['o'] ["output"]
    (ReqArg (\path opt -> opt {optOutput = writeFile path}) "FILE")
    "Redirect output to FILE."
  , Option ['t'] ["template"]
    (ReqArg (\syms opt -> opt {optTemplate = splitOn ":" syms}) "SYMBOLS")
    "Use SYMBOLS as the starting symbols (\"S\" is the default.)."
  , Option ['v'] ["version"]
    (NoArg (\opt -> opt {optVersion = True}))
    "Display version information."
  ]

main :: IO ()
main = do
  -- argument parsing
  args <- getArgs
  let (actions, params, errs) = getOpt RequireOrder options args

  when (not $ null errs) $ do
    hPutStrLn stderr (unlines errs)
    failUsageInfo

  let getOptions = foldl ( . ) id actions
      Options { optHelp     = help
              , optOutput   = output
              , optNumber   = number
              , optTemplate = template
              , optVersion  = version
              } = getOptions defaults

  when version $ do
    putStrLn (releaseInfo "babel" releaseVersion)
    exitSuccess

  when help $ do
    putStrLn (usageInfo header options)
    exitSuccess

  when (null params) $ do
    hPutStrLn stderr "You must provide a grammar file."
    failUsageInfo

  -- Load the grammar file
  description <- readFile (head params)
  case parseCFG description of
    Left  err -> hPutStrLn stderr (show err)
    Right cfg -> do
      let expansion = concat <$> mapM (expand cfg) template
      results <- replicateM number expansion
      output (unlines results)
  where
    header = "Usage: babel [OPTION...] GRAMMAR_FILE"
    releaseInfo pgm ver = pgm ++ " " ++ showVersion ver
    failUsageInfo = putStrLn (usageInfo header options) >> exitFailure
