{-# LANGUAGE DuplicateRecordFields #-}

module Card
  ( Card(..)
  , MagicCard(..)
  , MonsterCard(..)
  )
where

data Card = Monster MonsterCard
    | Magic MagicCard
    deriving (Show, Eq)
data MonsterCard = MonsterCard
    { name   :: String
    , attack :: Int
    , hp     :: Int
    }
    deriving (Show, Eq)
data MagicCard = MagicCard
    { name :: String
    }
    deriving (Show, Eq)
