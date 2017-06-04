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

module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

import Control.Monad(when, replicateM)

-- | The Options record holds a representation of the runtime configuration.
data Options = Options { optHelp    :: Bool
                       , optNumber  :: Int
                       , optOutput  :: (String -> IO ())
                       , optDefFile :: FilePath
                       }

-- | @defaults@ is the default runtime configuration record.
defaults = Options { optHelp    = False
                   , optNumber  = 1
                   , optOutput  = putStrLn
                   , optDefFile = "./babel.def"
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
  , Option ['d'] ["def"]
    (ReqArg (\str opt -> opt {optDefFile = str}) "FILE")
    "Use the definition provided in FILE."
  , Option ['o'] ["output"]
    (ReqArg (\path opt -> opt {optOutput = writeFile path}) "FILE")
    "Redirect output to FILE."
  ]

main :: IO ()
main = do
  -- argument parsing
  args <- getArgs
  let (actions, params, errs) = getOpt RequireOrder options args

  when (not $ null errs) $ do
    hPutStrLn stderr (unlines errs)
    putStrLn (usageInfo header options)
    exitFailure

  let getOptions = foldl ( . ) id actions
      Options { optHelp = help
              , optOutput = output
              , optNumber = number
              , optDefFile = defFile
              } = getOptions defaults

  when help $ do
    putStrLn (usageInfo header options)
    exitSuccess

  where
    header = "Usage: babel [OPTION...] PATTERN"