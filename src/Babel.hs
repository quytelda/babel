module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import qualified Data.Map as Map

data Options = Options { optHelp :: Bool
                       , optQuiet :: Bool
                       , optNumber :: Int
                       , optOutput :: (String -> IO ())
                       , optDefFile :: FilePath
                       }

defaults :: Options
defaults = Options { optHelp = False
                   , optQuiet = False
                   , optNumber = 1
                   , optOutput = putStrLn
                   , optDefFile = "./babel.defs"
                   }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"] (NoArg (\opt -> opt {optHelp = True}))
    "Display help message."
  , Option ['q'] ["quit"] (NoArg (\opt -> opt {optQuiet = True}))
    "Supress output."
  , Option ['n'] [] (ReqArg (\str opt -> opt {optNumber = (read str :: Int)}) "N")
    "Generate N sequences."
  , Option ['d'] ["defs"] (ReqArg (\str opt -> opt {optDefFile = str}) "FILE")
    "Use definitions in FILE."
  , Option ['o'] ["output"] (ReqArg (\path opt -> opt {optOutput = writeFile path}) "FILE")
    "Output to FILE."
  ]
  where
    writeFile path str = do
      handle <- openFile path WriteMode
      hPutStrLn handle str
      hClose handle

main :: IO ()
main = do
  args <- getArgs

  let (actions, params, errs) = getOpt RequireOrder options args

  let opts = foldl ( . ) id actions

  return ()
  where
    header = "babel [options] <pattern>"
