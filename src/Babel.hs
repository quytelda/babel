module Main where

import System.IO
import System.Environment
import System.Console.GetOpt

import qualified Data.Map as Map

data Options = Options { optHelp :: Bool
                       , optQuiet :: Bool
                       , optNumber :: Int
                       , optOutput :: (String -> IO ())
                       , optConfig :: FilePath
                       }
defaults :: Options
defaults = Options { optHelp = False
                   , optQuiet = False
                   , optNumber = 1
                   , optOutput = putStrLn
                   , optConfig = "./babel.defs"
                   }
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"] (NoArg (\opt -> opt {optHelp = True}))
    "Display help message."
  , Option ['q'] ["quit"] (NoArg (\opt -> opt {optQuiet = True}))
    "Supress output."
  , Option ['n'] [] (ReqArg (\str opt -> opt {optNumber = (read str :: Int)}) "N")
    "Generate N sequences."
  ]

main :: IO ()
main = do
  args <- getArgs

  let (actions, params, errs) = getOpt RequireOrder options args

  let opts = foldl ( . ) id actions

  putStrLn $ usageInfo options
  return ()
  where
    header = "babel [options] <pattern>"
