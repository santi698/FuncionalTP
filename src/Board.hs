module Board
  ( Board
  , newBoard
  , (!?)
  , play
  )
where

import           Data.Array.IArray (Array, assocs, elems, listArray, (!), (//))
import           Data.List         (find)
import           Data.Maybe        (Maybe, isNothing)

data Board a = Board (Array Int (Maybe a))
    deriving Eq

showBoardElem :: (Show a) => Maybe a -> String
showBoardElem (Just e) = show e
showBoardElem Nothing  = "_"

instance Show a => Show (Board a) where
  show (Board array) = "Board\n  " ++ (show $ map showBoardElem $ elems array)

newBoard :: Int -> Board a
newBoard size = Board (listArray (1, size) [ Nothing | x <- [1 .. size] ])

(!?) :: (Board a) -> Int -> Maybe a
(Board array) !? i = array ! i

emptyCells :: (Board a) -> Int
emptyCells (Board array) =
  foldr (\a -> if isNothing a then (1 +) else id) 0 array

update :: Board a -> [(Int, Maybe a)] -> Board a
update (Board array) updates = Board (array // updates)

updateAt :: Int -> Board a -> (Maybe a -> Maybe a) -> Board a
updateAt i (Board array) f = Board $ array // [(i, f $ array ! i)]

play :: a -> Board a -> Board a
play card board = case findFirstEmpty board of
  Just i  -> board `update` [(i, Just card)]
  Nothing -> board

findFirstEmpty :: Board a -> Maybe Int
findFirstEmpty (Board array) = case firstEmptyAssoc of
  Nothing     -> Nothing
  Just (i, v) -> Just i
  where firstEmptyAssoc = find (isNothing . snd) (assocs array)
