module Deck
  ( Deck(Deck)
  , draw
  , drawN
  , shuffle
  )
where

import           Control.Monad.Random (evalRandIO)
import qualified Shuffle              as S

data Deck a = Deck [a]
    deriving (Show, Eq)

draw :: Deck a -> (a, Deck a)
draw (Deck (x : xs)) = (x, (Deck xs))

drawN :: Deck a -> Int -> ([a], Deck a)
drawN (Deck cards) n = (drawnCards, Deck $ newCards) where (drawnCards, newCards) = splitAt n cards

shuffle :: Deck a -> IO (Deck a)
shuffle (Deck xs) = do
  ys <- evalRandIO $ S.shuffle xs
  return $ Deck (ys)
