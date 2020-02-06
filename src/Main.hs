{-# LANGUAGE DuplicateRecordFields #-}

import qualified NetworkController as NetworkController
import           System.IO         (BufferMode (NoBuffering), getLine,
                                    hSetBuffering, print, stdout)
import qualified UI

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for online haskell runner
  controller <- NetworkController.start 4242
  NetworkController.broadcast controller "Waiting for players to connect"
  NetworkController.waitForPlayers controller
  NetworkController.broadcast controller "All players ready"
  UI.start controller
