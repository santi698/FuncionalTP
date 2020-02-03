module Deck
  ( Deck(Deck)
  , draw
  , shuffle
  )
where

import           Card                 (Card)
import           Control.Monad.Random (evalRandIO)
import qualified Shuffle              as S

data Deck = Deck [Card]
    deriving (Show, Eq)

draw :: Deck -> (Card, Deck)
draw (Deck (x : xs)) = (x, (Deck xs))

shuffle :: Deck -> IO Deck
shuffle (Deck xs) = do
  ys <- evalRandIO $ S.shuffle xs
  return $ Deck (ys)
