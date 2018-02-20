module Random where

import System.Random
import Crypto.Number.Generate

{-| Randomly select an element from a list using the random number
function randRange.  randRange must be able to select a random Int
from a provided range.
If the list is empty, this function returns Nothing. -}
pickRIO :: (Monad  m) => ((Int, Int) -> m Int) -> [a] -> m (Maybe a)
pickRIO randRange [] = return Nothing
pickRIO randRange xs = let min = 0
                           max = length xs - 1
                       in randRange (min, max)
                          >>= return . Just . (xs !!)

randomCryptIO :: (Random a, Integral a) => (a, a) -> IO a
randomCryptIO (low, hi) = do
  integer <- generateBetween (toInteger low) (toInteger hi)
  return (fromIntegral integer)
