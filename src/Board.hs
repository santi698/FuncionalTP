module Board
  ( Board
  , newBoard
  , fromList
  , get
  , add
  , updateAt
  , setAt
  , fmap
  )
where

import           Data.Array.IArray (Array, assocs, elems, listArray, (!), (//))
import           Data.Either
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
newBoard size = fromList [ Nothing | x <- [1 .. size] ]

fromList :: [Maybe a] -> Board a
fromList list = Board (listArray (1, length list) list)

get :: (Board a) -> Int -> Maybe a
get (Board array) i = array ! i

emptyCells :: (Board a) -> Int
emptyCells (Board array) =
  foldr (\a -> if isNothing a then (1 +) else id) 0 array

update :: Board a -> [(Int, Maybe a)] -> Board a
update (Board array) updates = Board (array // updates)

updateAt :: Int -> Board a -> (Maybe a -> Maybe a) -> Board a
updateAt i (Board array) f = Board $ array // [(i, f $ array ! i)]

setAt :: Int -> Board a -> Maybe a -> Board a
setAt i (Board array) v = Board $ array // [(i, v)]

add :: a -> Board a -> Either String (Board a)
add card board = case findFirstEmpty board of
  Just i  -> Right $ board `update` [(i, Just card)]
  Nothing -> Left "Board is full"

instance Functor Board where
  fmap f (Board array) = Board (fmap (fmap f) array)

findFirstEmpty :: Board a -> Maybe Int
findFirstEmpty (Board array) = fmap fst firstEmptyAssoc
  where firstEmptyAssoc = find (isNothing . snd) (assocs array)
