module Random where

import Crypto.Number.Generate
import System.Random

{-| Randomly select an element from a list using the random number
function randRange.  randRange must be able to select a random Int
from a provided range.
If the list is empty, this function returns Nothing. -}
pickRIO :: ((Int, Int) -> IO Int) -> [a] -> IO (Maybe a)
pickRIO randRange [] = return Nothing
pickRIO randRange xs = let min = 0
                           max = length xs - 1
                       in randRange (min, max)
                          >>= return . Just . (xs !!)
