{-# LANGUAGE DuplicateRecordFields #-}

import           Data.Maybe (Maybe(Just, Nothing))
import           System.IO (hSetBuffering, print, getLine, BufferMode(NoBuffering), stdout)
import           Game (GameState(currentTurn), runAction, PlayerAction(EndTurn), newGame, Player(Player1, Player2))
import qualified NetworkController as NetworkController

x -: f = f x

main = do
  hSetBuffering stdout NoBuffering -- for online haskell runner
  controller <- NetworkController.start 4242
  NetworkController.broadcast controller "Waiting for players to connect"
  NetworkController.waitForPlayers controller
  NetworkController.broadcast controller "All players ready"
  gameLoop (newGame [] []) controller

gameLoop :: GameState -> NetworkController.NetworkController -> IO ()
gameLoop state controller = do
  NetworkController.broadcast controller $ show state
  NetworkController.broadcast controller $ "It's " ++ (show $ currentTurn state) ++ " turn."
  action <- getAction controller $ currentTurn state
  case action of
    Nothing -> do
      NetworkController.send controller NetworkController.Player1 "Invalid action"
      gameLoop state controller
    Just action -> gameLoop (state -: runAction action) controller

getAction controller Player1 = do
  NetworkController.send controller NetworkController.Player1 "What do you want to do?"
  input <- fmap snd $ NetworkController.receive controller
  return $ userInputToAction $ words input

getAction controller Player2 = do
  NetworkController.send controller NetworkController.Player2 "What do you want to do?"
  input <- fmap snd $ NetworkController.receive controller
  return $ userInputToAction $ words input


userInputToAction ["EndTurn"] = Just EndTurn
userInputToAction _ = Nothing