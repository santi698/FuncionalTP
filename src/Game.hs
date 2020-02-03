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
  )
where

import           Board             (Board, newBoard, (!?))
import           Card              (Card, MagicCard, MonsterCard (attack))
import           Data.Array.IArray
import           Data.List
import           Data.Maybe
import           Deck              (Deck (Deck), draw, shuffle)
import           System.IO

data GameState = GameState
    { playersStates :: (PlayerState, PlayerState)
    , currentTurn   :: Turn
    }
    deriving (Show, Eq)

class (Render a) where
  render :: a -> String

instance Render GameState where
  render (GameState (player1State, player2State) currentTurn) =
    (render $ board player1State)
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

playerState :: GameState -> Player -> PlayerState
playerState state Player1 = fst $ playersStates state
playerState state Player2 = snd $ playersStates state

type Turn = Player
data Player = Player1
    | Player2
    deriving (Show, Eq)

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

data PlayerState = PlayerState
    { hand  :: Hand
    , deck  :: Deck
    , board :: Board MonsterCard
    , hp    :: Int
    }
    deriving (Show, Eq)

type Hand = [Card]
type CanAttack = Bool
type MonsterSlot = Maybe MonsterCard
type MonsterSlots = [MonsterSlot]
type MonsterSlotIndex = Int
data AttackSource = SourceMonster MonsterSlotIndex
    deriving (Show, Eq)
data AttackTarget = TargetMonster MonsterSlotIndex
    | EnemyHero
    deriving (Show, Eq)

doAttack :: AttackSource -> AttackTarget -> GameState -> GameState
doAttack (SourceMonster idx) EnemyHero (GameState (player1State, player2State) Player1)
  = GameState
    ( player1State
    , reduceHp player2State (attack $ findInBoard idx player1State)
    )
    Player1

doAttack (SourceMonster idx) EnemyHero (GameState (player1State, player2State) Player2)
  = GameState
    ( reduceHp player1State (attack $ findInBoard idx player2State)
    , player2State
    )
    Player2

reduceHp :: PlayerState -> Int -> PlayerState
reduceHp (PlayerState hand deck board hp) amount =
  PlayerState hand deck board (hp - amount)

reducePlayerHp Player1 (GameState (player1State, player2State) turn) amount =
  GameState (reduceHp player1State amount, player2State) turn

reducePlayerHp Player2 (GameState (player1State, player2State) turn) amount =
  GameState (player1State, reduceHp player2State amount) turn

findInBoard idx player = fromJust $ (board player) !? idx

endTurn (GameState playersStates turn) =
  GameState playersStates (nextPlayer turn)

data PlayerAction = Attack AttackSource AttackTarget
    | PlayCardFromHand Int
    | EndTurn
    deriving (Show, Eq)

runAction :: PlayerAction -> GameState -> GameState
runAction (Attack src tgt) gameState = doAttack src tgt gameState
runAction (EndTurn       ) gameState = endTurn gameState

newGame :: Deck -> Deck -> IO GameState
newGame player1Deck player2Deck = do
  player1Deck <- shuffle player1Deck
  player2Deck <- shuffle player2Deck
  return GameState
    { playersStates =
      ( PlayerState
        { hand  = []
        , deck  = player1Deck
        , board = newBoard 5
        , hp    = 30
        }
      , PlayerState
        { hand  = []
        , deck  = player2Deck -- TODO shuffle
        , board = newBoard 5
        , hp    = 30
        }
      )
    , currentTurn   = Player1
    }
