{-# LANGUAGE DuplicateRecordFields #-}

module Card
  ( Card(..)
  , MagicCard(..)
  , MonsterCard(..)
  )
where

data Card = Monster MonsterCard
    | Magic MagicCard
    deriving (Eq)

instance Show Card where
    show (Monster m) = show m
    show (Magic m)   = show m

data MonsterCard = MonsterCard
    { name   :: String
    , attack :: Int
    , hp     :: Int
    }
    deriving (Eq)

instance Show MonsterCard where
    show (MonsterCard name attack hp) =
        "Monster(" ++ name ++ " " ++ (show attack) ++ "/" ++ (show hp) ++ ")"

data MagicCard = MagicCard
    { name        :: String
    , description :: String
    }
    deriving (Eq)

instance Show MagicCard where
    show (MagicCard name description) = "Magic(\"" ++ name ++ "\" " ++ description  ++ ")"
