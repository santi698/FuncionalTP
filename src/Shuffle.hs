module Shuffle
  ( shuffle
  )
where

import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.ST
import           Data.Array.ST
import           GHC.Arr
import           System.Random

shuffle :: RandomGen g => [a] -> Rand g [a]
shuffle xs = do
  let l = length xs
  rands <- forM [0 .. (l - 2)] $ \i -> getRandomR (i, l - 1)
  let
    ar = runSTArray $ do
      ar <- thawSTArray $ listArray (0, l - 1) xs
      forM_ (zip [0 ..] rands) $ \(i, j) -> do
        vi <- readSTArray ar i
        vj <- readSTArray ar j
        writeSTArray ar j vi
        writeSTArray ar i vj
      return ar
  return (elems ar)
