module UI(start, GameController(..), Player(..), Msg) where

import           Data.Maybe   (Maybe (Just, Nothing))
import qualified ExampleDecks
import           Game         (AttackSource (SourceMonster),
                               AttackTarget (EnemyHero, TargetMonster),
                               Deck (Deck), GameState (currentTurn),
                               Player (Player1, Player2), PlayerAction (..),
                               PlayerState (hand), newGame, playerState, render,
                               runAction)

type Msg = (Player, String)

class GameController a where
  send :: a -> Player -> String -> IO ()
  broadcast :: a -> String -> IO ()
  receive :: a -> IO Msg

x -: f = f x

start :: (GameController a) => a -> IO ()
start controller = do
  gameState <- newGame ExampleDecks.exampleDeck1 ExampleDecks.exampleDeck1
  gameLoop gameState controller

gameLoop :: (GameController a) => GameState -> a -> IO ()
gameLoop state controller = do
  broadcast controller $ render state
  action <- getAction controller turn (playerState state turn)
  case action of
    Nothing -> do
      send
        controller
        Player1
        "Invalid action"
      gameLoop state controller
    Just action -> gameLoop (state -: runAction action) controller
  where turn = currentTurn state

getAction :: (GameController a) => a -> Player -> PlayerState -> IO (Maybe PlayerAction)
getAction controller Player1 playerState = do
  send
    controller
    Player1
    $
    "Your hand is " ++ (show $ hand playerState)
  send
    controller
    Player1
    "What do you want to do?"
  input <- fmap snd $ receive controller
  return $ userInputToAction $ words input

getAction controller Player2 playerState = do
  send
    controller
    Player2
    $ "Your hand is" ++
      (show $ hand playerState)
  send
    controller
    Player2
    "What do you want to do?"
  input <- fmap snd $ receive controller
  return $ userInputToAction $ words input

userInputToAction :: [String] -> Maybe PlayerAction
userInputToAction ["DrawCard"]            = Just DrawCard
userInputToAction ["PlayCardFromHand", i] = Just (PlayCardFromHand (read i))
userInputToAction ["Attack", src, "EnemyHero"] =
    Just (Attack (SourceMonster $ read src) (EnemyHero))
userInputToAction ["Attack", src, tgt] =
    Just (Attack (SourceMonster $ read src) (TargetMonster $ read tgt))
userInputToAction ["EndTurn"]             = Just EndTurn
userInputToAction _                       = Nothing
