{-# LANGUAGE DuplicateRecordFields #-}

module Game (GameState(currentTurn), runAction, PlayerAction(..), newGame, Player(..)) where

import Data.Maybe
import Data.Array.IArray
import Data.List
import System.IO
import Board (Board, (!?), newBoard)

data GameState = GameState {
  playersStates :: (PlayerState, PlayerState),
  currentTurn :: Turn
} deriving (Show, Eq)

type Turn = Player
data Player = Player1 | Player2 deriving (Show, Eq)

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

data PlayerState = PlayerState {
  hand :: Hand,
  deck :: Deck,
  board :: Board MonsterCard,
  hp :: Int
} deriving (Show, Eq)

type Hand = [Card]
type Deck = [Card]
data Card = Monster MonsterCard | Magic MagicCard deriving (Show, Eq)
data MonsterCard = MonsterCard { name :: String, attack :: Int, hp :: Int } deriving (Show, Eq)
data MagicCard = MagicCard { name :: String } deriving (Show, Eq)
type CanAttack = Bool
type MonsterSlot = Maybe MonsterCard
type MonsterSlots = [MonsterSlot]
type MonsterSlotIndex = Int
data AttackSource = SourceMonster MonsterSlotIndex deriving (Show, Eq)
data AttackTarget = TargetMonster MonsterSlotIndex | EnemyHero deriving (Show, Eq)

doAttack :: AttackSource -> AttackTarget -> GameState -> GameState
doAttack (SourceMonster idx) EnemyHero (GameState (player1State, player2State) Player1) =
  GameState (player1State, reduceHp player2State (attack $ findMonster idx player1State)) Player1

doAttack (SourceMonster idx) EnemyHero (GameState (player1State, player2State) Player2) =
  GameState (reduceHp player1State (attack $ findMonster idx player2State), player2State) Player2

reduceHp :: PlayerState -> Int -> PlayerState
reduceHp (PlayerState hand deck board hp) amount = PlayerState hand deck board (hp - amount)

reducePlayerHp Player1 (GameState (player1State, player2State) turn) amount =
  GameState (reduceHp player1State amount, player2State) turn
  
reducePlayerHp Player2 (GameState (player1State, player2State) turn) amount =
  GameState (player1State, reduceHp player2State amount) turn

findMonster idx player = fromJust $ (board player) !? idx

endTurn (GameState playersStates turn) = GameState playersStates (nextPlayer turn)

data PlayerAction = Attack AttackSource AttackTarget | PlayCardFromHand Int | EndTurn deriving (Show, Eq)

runAction :: PlayerAction -> GameState -> GameState
runAction (Attack src tgt) gameState = doAttack src tgt gameState
runAction (EndTurn) gameState = endTurn gameState

newGame player1Deck player2Deck = GameState {
  playersStates = (
    PlayerState {
      hand = [],
      deck = player1Deck,
      board = newBoard 5,
      hp = 30
    },
    PlayerState {
      hand = [],
      deck = player2Deck, -- TODO shuffle
      board = newBoard 5,
      hp = 30
    }
  ),
  currentTurn = Player1
}