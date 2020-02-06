{-# LANGUAGE DuplicateRecordFields #-}

module Game
  ( GameState(..)
  , PlayerState(..)
  , playerState
  , runAction
  , PlayerAction(..)
  , newGame
  , Player(..)
  , Deck(..)
  , render
  , AttackSource(..)
  , AttackTarget(..)
  , Card (MonsterCard, SpellCard)
  , Monster (..)
  , Spell (..)
  , raiseAttack
  )
where

import           Board                   (Board, add, fmap, get, newBoard,
                                          setAt)
import           Control.Monad           (when)
import           Data.Array.IArray
import           Data.Either
import           Data.Either.Combinators (maybeToRight)
import           Data.List
import           Data.Maybe
import           Deck                    (Deck (Deck), draw, drawN, shuffle)
import           System.IO

data GameState = GameState
    { player1State :: PlayerState
    , player2State :: PlayerState
    , currentTurn  :: Turn
    }
    deriving (Show)

data EndCondition = Winner Player
    | Tie

-- checkEndCondition :: GameState -> Maybe EndCondition
-- checkEndCondition (GameState player1State player2State _) =

data PlayerState = PlayerState
    { hand        :: Hand
    , deck        :: Deck Card
    , board       :: Board Monster
    , playerHp    :: Int
    , currentMana :: Int
    , maxMana     :: Int
    }
    deriving (Show)

newPlayerState deck = do
  deckS <- shuffle deck
  let (hand, deckAfterDraw) = drawN deckS 5
  return PlayerState { hand        = hand
                     , deck        = deckAfterDraw
                     , board       = newBoard 5
                     , playerHp    = 30
                     , currentMana = 1
                     , maxMana     = 1
                     }

class (Render a) where
  render :: a -> String

instance Render GameState where
  render (GameState player1State player2State currentTurn) =
    "Player 1 "
      ++ (render player1State)
      ++ "\nPlayer 2 "
      ++ (render player2State)
      ++ "\n"
      ++ (render $ board player1State)
      ++ "\n"
      ++ (render $ board player2State)
      ++ "\n"
      ++ "It's "
      ++ show currentTurn
      ++ "turn.\n"

instance (Show a) => (Render [a]) where
  render s = show s

instance (Show a) => (Render (Board a)) where
  render board = show board

instance Render PlayerState where
  render playerState =
    "HP: "
      ++ (show $ playerHp playerState)
      ++ " Mana: "
      ++ (show $ currentMana playerState)
      ++ "/"
      ++ (show $ maxMana playerState)

playerState :: GameState -> Player -> PlayerState
playerState state Player1 = player1State state
playerState state Player2 = player2State state

type Turn = Player
data Player = Player1
    | Player2
    deriving (Show, Eq)

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

type Hand = [Card]
playCard :: Hand -> Int -> (Card, Hand)
playCard xs i = let (start, end) = splitAt i xs in (head end, start ++ drop 1 end)
type MonsterSlotIndex = Int
data AttackSource = SourceMonster MonsterSlotIndex
    deriving (Show, Eq, Read)
data AttackTarget = TargetMonster MonsterSlotIndex
    | EnemyHero
    deriving (Show, Eq, Read)

doAttack :: AttackSource -> AttackTarget -> GameState -> Either String GameState
doAttack (SourceMonster i) EnemyHero gameState
  = case currentTurn gameState of
    Player1 -> do
      let b = board p1s
      m <- maybeToRight "Source monster not found" $ get b i
      when (not $ canAttack m) $ Left "The monster was just played or has already attacked"
      return $ gameState { player1State = p1s { board = setAt i b (Just $ alreadyAttacked m) }
                         , player2State = (reducePlayerStateHp p2s (attack m))
                         }
    Player2 -> do
      let b = board p2s
      m <- maybeToRight "Source monster not found" $ get b i
      return $ gameState { player1State = (reducePlayerStateHp p1s (attack m))
                         , player2State = p2s { board = setAt i b (Just $ alreadyAttacked m)}
                         }
  where p1s = player1State gameState
        p2s = player2State gameState

doAttack (SourceMonster i) (TargetMonster j) gameState
  = case currentTurn gameState of
    Player1 -> do
                m1 <- maybeToRight "Source monster not found" $ get b1 i
                m2 <- maybeToRight "Target monster not found" $ get b2 j
                when (not $ canAttack m1) $ Left "The monster was just played or has already attacked"
                let (m1After, m2After) = fight m1 m2
                return $ gameState { player1State = p1s { board = setAt i b1 m1After }
                                   , player2State = p2s { board = setAt j b2 m2After }
                                   }
    Player2 -> do
               m1 <- maybeToRight "Target monster not found" $ get b1 j
               m2 <- maybeToRight "Source monster not found" $ get b2 i
               when (not $ canAttack m2) $ Left "The monster was just played or has already attacked"
               let (m2After, m1After) = fight m2 m1
               return $ gameState { player1State = p1s { board = setAt j b1 m1After }
                                  , player2State = p2s { board = setAt i b2 m2After }
                                  }
    where b1 = board p1s
          b2 = board p2s
          p1s = player1State gameState
          p2s = player2State gameState

reducePlayerStateHp :: PlayerState -> Int -> PlayerState
reducePlayerStateHp playerState amount =
  playerState { playerHp = (playerHp playerState) - amount }

reducePlayerHp :: Player -> GameState -> Int -> GameState
reducePlayerHp Player1 gameState amount =
  gameState { player1State = reducePlayerStateHp (player1State gameState) amount }

reducePlayerHp Player2 gameState amount =
  gameState { player2State = reducePlayerStateHp (player2State gameState) amount }

endTurn gameState = case currentTurn gameState of
  Player1 ->
    Right $ gameState { currentTurn = nextPlayer $ currentTurn gameState
                      , player1State = raiseAndRecoverMana $ updateCanAttack $ player1State gameState
                      , player2State = updateCanAttack $ player2State gameState
                      }
  Player2 ->
    Right $ gameState { currentTurn = nextPlayer $ currentTurn gameState
                      , player1State = updateCanAttack $ player1State gameState
                      , player2State = raiseAndRecoverMana $ updateCanAttack $ player2State gameState
                      }

updateCanAttack :: PlayerState -> PlayerState
updateCanAttack playerState =
  playerState { board = fmap (\v -> v { canAttack = True }) $ board playerState }

raiseAndRecoverMana :: PlayerState -> PlayerState
raiseAndRecoverMana playerState = playerState { maxMana = newMaxMana, currentMana = newMaxMana }
                                  where newMaxMana = (maxMana playerState) + 1

drawCard (GameState player1State player2State Player1) =
  Right $ GameState player1State { deck = newDeck
                                 , hand = newHand
                                 }
                    player2State
                    Player1
  where (drawnCard, newDeck) = draw $ deck player1State
        newHand = drawnCard:(hand player1State)

drawCard (GameState player1State player2State Player2) =
  Right $
  GameState player1State
            player2State { deck = newDeck
                         , hand = newHand
                         }
            Player2
  where (drawnCard, newDeck) = draw $ deck player2State
        newHand = drawnCard:(hand player2State)

playCardFromHand i (GameState player1State player2State Player1) =
  let (card, newHand) = playCard (hand player1State) i
  in case card of
    (MonsterCard monster) -> do
      when ((costM monster) > (currentMana player1State)) $ Left $ "Not enough mana"
      let newMana = (currentMana player1State) - (costM monster)
      newBoard <- add monster $ board player1State
      return $ GameState player1State { hand = newHand
                                      , board = newBoard
                                      , currentMana = newMana
                                      }
                         player2State
                         Player1
    (SpellCard spell) -> do
      when ((cost spell) > (currentMana player1State)) $ Left $ "Not enough mana"
      let newMana = (currentMana player1State) - (cost spell)
      return $ (effect spell) (GameState player1State { hand = newHand, currentMana = newMana } player2State Player1)

playCardFromHand i (GameState player1State player2State Player2) =
  let (card, newHand) = playCard (hand player2State) i
  in case card of
    (MonsterCard monster) -> do
      when ((costM monster) > (currentMana player2State)) $ Left $ "Not enough mana"
      let newMana = (currentMana player2State) - (costM monster)
      newBoard <- add monster $ board player2State
      return $ GameState player1State
                         player2State { hand = newHand
                                      , board = newBoard
                                      , currentMana = newMana
                                      }
                         Player2
    (SpellCard spell) -> do
      when ((cost spell) > (currentMana player2State)) $ Left $ "Not enough mana"
      let newMana = (currentMana player2State) - (cost spell)
      return $ (effect spell) (GameState player1State player2State { hand = newHand, currentMana = newMana } Player2)

data PlayerAction = Attack AttackSource AttackTarget
    | PlayCard Int
    | EndTurn
    deriving (Show, Eq, Read)

runAction :: PlayerAction -> GameState -> Either String GameState
runAction (Attack src tgt) gameState     = doAttack src tgt gameState
runAction (EndTurn       ) gameState     = do
  newState <- endTurn gameState
  drawCard newState
runAction (PlayCard i) gameState = playCardFromHand i gameState

newGame :: Deck Card -> Deck Card -> IO GameState
newGame player1Deck player2Deck = do
  player1State <- newPlayerState player1Deck
  player2State <- newPlayerState player2Deck
  return $ GameState player1State player2State Player1

-- Card

data Card = MonsterCard Monster
    | SpellCard Spell

instance Show Card where
    show (MonsterCard m) = show m
    show (SpellCard m)   = show m

data Monster = Monster
    { name      :: String
    , attack    :: Int
    , hp        :: Int
    , costM     :: Int
    , canAttack :: Bool
    }

kills m1 m2 = (attack m1) >= (hp m2)

fight m1 m2
  | kills m1 m2 && kills m2 m1 = (Nothing, Nothing)
  | kills m1 m2 = (Just $ alreadyAttacked (reduceHp m1 (attack m2)), Nothing)
  | kills m2 m1 = (Nothing, Just $ reduceHp m2 (attack m1))
  | otherwise = (Just $ alreadyAttacked (reduceHp m1 (attack m2)), Just $ reduceHp m2 (attack m1))

reduceHp m x = m { hp = (hp m) - x}
raiseAttack x m = m { attack = (attack m) + x}
alreadyAttacked m = m { canAttack = False }

instance Show Monster where
    show (Monster name attack hp cost canAttack) =
        "Monster("
        ++ name
        ++ " "
        ++ (show attack)
        ++ "/"
        ++ (show hp)
        ++ " ["
        ++ (show cost)
        ++ "]"
        ++ (if canAttack then "" else " Z")
        ++ ")"

data Spell = Spell
    { name        :: String
    , description :: String
    , cost        :: Int
    , effect      :: GameState -> GameState
    }

instance Show Spell where
    show (Spell name description cost _) =
      "Magic(\"" ++ name ++ "\" " ++ description  ++ " [" ++ (show cost) ++ "])"
