module Main where

import System.IO
import System.Environment
import System.Exit
import System.Console.GetOpt

import Control.Monad(when)

import qualified Data.Map as Map

-- |The Options structure holds the results of parsed command-line arguments
data Options = Options { optHelp :: Bool
                       , optNumber :: Int
                       , optOutput :: (String -> IO ())
                       , optDefFile :: FilePath
                       }

-- |The default runtime configuration
defaults :: Options
defaults = Options { optHelp = False
                   , optNumber = 1
                   , optOutput = putStrLn
                   , optDefFile = "./babel.defs"
                   }

-- |options describes the supported command-line arguments
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h'] ["help"] (NoArg (\opt -> opt {optHelp = True}))
    "Display help message."
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

-- Entry Point
main :: IO ()
main = do
  args <- getArgs

  let (actions, params, errs) = getOpt RequireOrder options args
      getOptions = foldl ( . ) id actions
      Options { optHelp = help
              , optOutput = output
              } = getOptions defaults

  when help $ do
    putStrLn $ usageInfo header options
    exitSuccess

  return ()
  where
    header = "babel [options] <pattern>"
