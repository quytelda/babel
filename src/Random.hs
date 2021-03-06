module Random where

import Crypto.Number.Generate

{-| Randomly select an element from a list using the random number
function randRange.  randRange must be able to select a random Int
from a provided range.
If the list is empty, this function returns Nothing. -}
pickRIO :: Monad m => ((Int, Int) -> m Int) -> [a] -> m (Maybe a)
pickRIO _         [] = return Nothing
pickRIO randRange xs = let minI = 0
                           maxI = length xs - 1
                       in randRange (minI, maxI)
                          >>= return . Just . (xs !!)

{-| A cryptographically secure version of 'System.Random.randomRIO'.
'randomCryptIO' generates a random number on the interval ['lo', 'hi']. -}
randomCryptIO :: Integral a => (a, a) -> IO a
randomCryptIO (lo, hi) = do
  integer <- generateBetween (toInteger lo) (toInteger hi)
  return (fromIntegral integer)
