module CFG where

import Prelude hiding (lookup)
import System.Random
import Data.Maybe
import Data.Map

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

{-| Randomly select an element from a list. -}
pickRIO :: [a] -> IO (Maybe a)
pickRIO [] = return Nothing
pickRIO xs = randomRIO (0, length xs - 1)
             >>= return . Just . (xs !!)
