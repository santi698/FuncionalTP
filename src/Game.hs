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
import           Deck              (Deck (Deck), draw, drawN, shuffle)
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

drawCard (GameState (player1State, player2State) Player1) =
  GameState ( player1State { deck = newDeck
                           , hand = newHand
                           }
            , player2State)
            Player1
  where (drawnCard, newDeck) = draw $ deck player1State
        newHand = drawnCard:(hand player1State)

drawCard (GameState (player1State, player2State) Player2) =
  GameState ( player1State
            , player2State { deck = newDeck
                           , hand = newHand
                           }
            )
            Player2
  where (drawnCard, newDeck) = draw $ deck player1State
        newHand = drawnCard:(hand player1State)

data PlayerAction = Attack AttackSource AttackTarget
    | PlayCardFromHand Int
    | EndTurn
    | DrawCard
    deriving (Show, Eq)

runAction :: PlayerAction -> GameState -> GameState
runAction (Attack src tgt) gameState = doAttack src tgt gameState
runAction (EndTurn       ) gameState = drawCard $ endTurn gameState
runAction (DrawCard) gameState       = drawCard gameState

newGame :: Deck -> Deck -> IO GameState
newGame player1Deck player2Deck = do
  player1DeckS <- shuffle player1Deck
  let (player1Hand, player1Deck) = drawN player1DeckS 5
  player2DeckS <- shuffle player2Deck
  let (player2Hand, player2Deck) = drawN player2DeckS 5
  return GameState
    { playersStates =
      ( PlayerState
        { hand  = player1Hand
        , deck  = player1Deck
        , board = newBoard 5
        , hp    = 30
        }
      , PlayerState
        { hand  = player2Hand
        , deck  = player2Deck
        , board = newBoard 5
        , hp    = 30
        }
      )
    , currentTurn   = Player1
    }
