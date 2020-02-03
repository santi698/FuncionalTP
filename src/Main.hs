{-# LANGUAGE DuplicateRecordFields #-}

import           Data.Maybe        (Maybe (Just, Nothing))
import qualified ExampleDecks
import           Game              (Deck (Deck), GameState (currentTurn),
                                    Player (Player1, Player2),
                                    PlayerAction (EndTurn), PlayerState (hand),
                                    newGame, playerState, render, runAction)
import qualified NetworkController as NetworkController
import           System.IO         (BufferMode (NoBuffering), getLine,
                                    hSetBuffering, print, stdout)

x -: f = f x

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for online haskell runner
  controller <- NetworkController.start 4242
  NetworkController.broadcast controller "Waiting for players to connect"
  NetworkController.waitForPlayers controller
  NetworkController.broadcast controller "All players ready"
  gameState <- newGame ExampleDecks.exampleDeck1 ExampleDecks.exampleDeck2
  gameLoop gameState controller

gameLoop :: GameState -> NetworkController.NetworkController -> IO ()
gameLoop state controller = do
  NetworkController.broadcast controller $ render state
  action <- getAction controller turn (playerState state turn)
  case action of
    Nothing -> do
      NetworkController.send
        controller
        NetworkController.Player1
        "Invalid action"
      gameLoop state controller
    Just action -> gameLoop (state -: runAction action) controller
  where turn = currentTurn state

getAction :: NetworkController.NetworkController -> Player -> PlayerState -> IO (Maybe PlayerAction)
getAction controller Player1 playerState = do
  NetworkController.send
    controller
    NetworkController.Player1
    $
    "Your hand is" ++
      (show $ hand playerState)
  NetworkController.send
    controller
    NetworkController.Player1
    "What do you want to do?"
  input <- fmap snd $ NetworkController.receive controller
  return $ userInputToAction $ words input

getAction controller Player2 playerState = do
  NetworkController.send
    controller
    NetworkController.Player2
    $ "Your hand is" ++
      (show $ hand playerState)
  NetworkController.send
    controller
    NetworkController.Player2
    "What do you want to do?"
  input <- fmap snd $ NetworkController.receive controller
  return $ userInputToAction $ words input

userInputToAction :: [String] -> Maybe PlayerAction
userInputToAction ["EndTurn"] = Just EndTurn
userInputToAction _           = Nothing
