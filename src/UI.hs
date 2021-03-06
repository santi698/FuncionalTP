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
  gameState <- newGame ExampleDecks.exampleDeck1 ExampleDecks.exampleDeck2
  gameLoop gameState controller

gameLoop :: (GameController a) => GameState -> a -> IO ()
gameLoop state controller = do
  broadcast controller $ render state
  action <- getAction controller currentPlayer (playerState state currentPlayer)
  case action of
    Nothing -> do
      send controller currentPlayer "Invalid action"
      gameLoop state controller
    Just action ->
      case newState of
        Left e -> do
          send controller currentPlayer e
          gameLoop state controller
        Right s -> gameLoop s controller
      where newState = state -: runAction action
  where currentPlayer = currentTurn state

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
    "What do you want to do?\nPossible actions: EndTurn, Attack from to, PlayCard index"
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
userInputToAction ["PlayCard", i] = Just (PlayCard ((read i) - 1))
userInputToAction ["Attack", src, "EnemyHero"] =
    Just (Attack (SourceMonster $ read src) (EnemyHero))
userInputToAction ["Attack", src, tgt] =
    Just (Attack (SourceMonster $ read src) (TargetMonster $ read tgt))
userInputToAction ["EndTurn"]             = Just EndTurn
userInputToAction _                       = Nothing
