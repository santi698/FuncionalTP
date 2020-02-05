{-# LANGUAGE DuplicateRecordFields #-}

module Card
  ( Card(..)
  , MagicCard(..)
  , MonsterCard(..)
  , fight
  , raiseAttack
  )
where

data Card = Monster MonsterCard
    | Magic MagicCard
    deriving (Eq)

instance Show Card where
    show (Monster m) = show m
    show (Magic m)   = show m

data MonsterCard = MonsterCard
    { name      :: String
    , attack    :: Int
    , hp        :: Int
    , canAttack :: Bool
    }
    deriving (Eq)

kills m1 m2 = (attack m1) >= (hp m2)

fight m1 m2
  | kills m1 m2 && kills m2 m1 = (Nothing, Nothing)
  | kills m1 m2 = (Just $ alreadyAttacked (reduceHp m1 (attack m2)), Nothing)
  | kills m2 m1 = (Nothing, Just $ reduceHp m2 (attack m1))
  | otherwise = (Just $ alreadyAttacked (reduceHp m1 (attack m2)), Just $ reduceHp m2 (attack m1))

reduceHp m x = m { hp = (hp m) - x}
raiseAttack m x = m { attack = (attack m) + x}
alreadyAttacked m = m { canAttack = False }

instance Show MonsterCard where
    show (MonsterCard name attack hp canAttack) =
        "Monster("
        ++ name
        ++ " "
        ++ (show attack)
        ++ "/"
        ++ (show hp)
        ++ (if canAttack then "" else " Z")
        ++ ")"

data MagicCard = MagicCard
    { name        :: String
    , description :: String
    }
    deriving (Eq)

instance Show MagicCard where
    show (MagicCard name description) = "Magic(\"" ++ name ++ "\" " ++ description  ++ ")"
