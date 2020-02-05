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
  )
where

import           Board                   (Board, add, fmap, get, newBoard,
                                          setAt)
import           Card                    (Card (Magic, Monster), MagicCard (..),
                                          MonsterCard (..), fight)
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
    deriving (Show, Eq)

data EndCondition = Winner Player
    | Tie

-- checkEndCondition :: GameState -> Maybe EndCondition
-- checkEndCondition (GameState player1State player2State _) =

data PlayerState = PlayerState
    { hand     :: Hand
    , deck     :: Deck
    , board    :: Board MonsterCard
    , playerHp :: Int
    }
    deriving (Show, Eq)

class (Render a) where
  render :: a -> String

instance Render GameState where
  render (GameState player1State player2State currentTurn) =
    "Player 1 HP: "
      ++ (show $ playerHp player1State)
      ++ "\nPlayer 2 HP: "
      ++ (show $ playerHp player2State)
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
type MonsterSlot = Maybe MonsterCard
type MonsterSlots = [MonsterSlot]
type MonsterSlotIndex = Int
data AttackSource = SourceMonster MonsterSlotIndex
    deriving (Show, Eq, Read)
data AttackTarget = TargetMonster MonsterSlotIndex
    | EnemyHero
    deriving (Show, Eq, Read)

doAttack :: AttackSource -> AttackTarget -> GameState -> Either String GameState
doAttack (SourceMonster idx) EnemyHero gameState
  = case currentTurn gameState of
    Player1 -> do
      m <- maybeToRight "Source monster not found" $ get (board p1s) idx
      when (not $ canAttack m) $ Left "The monster was just played or has already attacked"
      return $ gameState { player2State = (reduceHp p2s (attack m)) }
    Player2 -> do
      m <- maybeToRight "Source monster not found" $ get (board p2s) idx
      return $ gameState { player1State = (reduceHp p1s (attack m)) }
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

reduceHp :: PlayerState -> Int -> PlayerState
reduceHp playerState amount =
  playerState { playerHp = (playerHp playerState) - amount }

reducePlayerHp :: Player -> GameState -> Int -> GameState
reducePlayerHp Player1 gameState amount =
  gameState { player1State = reduceHp (player1State gameState) amount }

reducePlayerHp Player2 gameState amount =
  gameState { player2State = reduceHp (player1State gameState) amount }

endTurn gameState = Right $ gameState { currentTurn = nextPlayer $ currentTurn gameState
                                      , player1State = updateCanAttack $ player1State gameState
                                      , player2State = updateCanAttack $ player2State gameState
                                      }

updateCanAttack :: PlayerState -> PlayerState
updateCanAttack playerState =
  playerState { board = fmap (\v -> v { canAttack = True }) $ board playerState }

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
  where (drawnCard, newDeck) = draw $ deck player1State
        newHand = drawnCard:(hand player1State)

playCardFromHand i (GameState player1State player2State Player1) =
  let (card, newHand) = playCard (hand player1State) i
  in case card of
    (Monster monster) -> Right $
                         GameState player1State { hand = newHand
                                                , board = add monster $ board player1State
                                                }
                                   player2State
                                   Player1
    otherwise -> Left "Only monsters can be played"

playCardFromHand i (GameState player1State player2State Player2) =
  let (card, newHand) = playCard (hand player2State) i
  in case card of
    (Monster monster) -> Right $
                         GameState player1State
                                   player2State { hand = newHand
                                                , board = add monster $ board player2State
                                                }
                                   Player2
    otherwise -> Left "Only monsters can be played"

data PlayerAction = Attack AttackSource AttackTarget
    | PlayCardFromHand Int
    | EndTurn
    | DrawCard
    deriving (Show, Eq, Read)

runAction :: PlayerAction -> GameState -> Either String GameState
runAction (Attack src tgt) gameState     = doAttack src tgt gameState
runAction (EndTurn       ) gameState     = do
  newState <- endTurn gameState
  drawCard newState
runAction (DrawCard) gameState           = drawCard gameState
runAction (PlayCardFromHand i) gameState = playCardFromHand i gameState

newGame :: Deck -> Deck -> IO GameState
newGame player1Deck player2Deck = do
  player1DeckS <- shuffle player1Deck
  let (player1Hand, player1Deck) = drawN player1DeckS 5
  player2DeckS <- shuffle player2Deck
  let (player2Hand, player2Deck) = drawN player2DeckS 5
  return $ GameState PlayerState { hand  = player1Hand
                                 , deck  = player1Deck
                                 , board = newBoard 5
                                 , playerHp    = 30
                                 }
                     PlayerState { hand  = player2Hand
                                 , deck  = player2Deck
                                 , board = newBoard 5
                                 , playerHp    = 30
                                 }
                     Player1
