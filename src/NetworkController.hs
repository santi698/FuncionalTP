module NetworkController (
  main,
  start,
  broadcast,
  send,
  receive,
  waitForPlayers,
  NetworkController,
  Player(Player1,Player2)
  ) where

import Network.Socket (
  accept,
  bind,
  listen,
  close,
  socket,
  socketToHandle,
  setSocketOption,
  PortNumber,
  Socket,
  Family(AF_INET),
  SocketType(Stream),
  SocketOption(ReuseAddr),
  SockAddr(SockAddrInet))
import System.IO (
  hSetBuffering,
  hClose,
  hGetLine,
  hPutStrLn,
  IOMode(ReadWriteMode),
  BufferMode(NoBuffering),
  stdout)
import Control.Exception
import Control.Concurrent (
  newChan,
  readChan,
  writeChan,
  dupChan,
  forkIO,
  killThread,
  Chan,
  MVar,
  newMVar,
  readMVar,
  putMVar,
  takeMVar,
  swapMVar,
  newEmptyMVar,
  tryReadMVar,
  isEmptyMVar
  )
import Control.Monad (when)
import Control.Monad.Fix (fix)

main = do
  hSetBuffering stdout NoBuffering
  controller <- start 4242
  fix $ \loop -> do
    msg <- receive controller
    print msg
    printNetworkControllerState $ state controller
    loop

data Player = Player1 | Player2 deriving (Show)

type Msg = (Player, String)

data NetworkController = NetworkController {
  player1 :: Chan String,
  player2 :: Chan String,
  received :: Chan Msg,
  state :: NetworkControllerState
}

data PlayerState = PlayerState {
  ready :: MVar Bool
}

newPlayerState = do 
  ready <- newEmptyMVar
  return $ PlayerState ready

printPlayerState (PlayerState ready) = do
  isReady <- tryReadMVar ready
  putStr $ "Ready: " ++ show isReady

data NetworkControllerState = NetworkControllerState {
  player1State :: PlayerState,
  player2State :: PlayerState
}

newNetworkControllerState = do
  player1State <- newPlayerState
  player2State <- newPlayerState
  return NetworkControllerState { player1State = player1State, player2State = player2State }

getPlayerState Player1 = player1State
getPlayerState Player2 = player2State

printNetworkControllerState (NetworkControllerState player1 player2)  = do
  putStrLn "State:"
  putStr "\tPlayer1: { "
  printPlayerState player1
  putStr " }\n\tPlayer2: { "
  printPlayerState player2
  putStr " }\n"
  

newNetworkController :: IO NetworkController
newNetworkController = do
  player1Chan <- newChan
  player2Chan <- newChan
  receiveChan <- newChan
  state <- newNetworkControllerState
  return $ NetworkController player1Chan player2Chan receiveChan state

send :: NetworkController -> Player -> String -> IO ()
send (NetworkController p1 _ _ _) Player1 msg = writeChan p1 msg
send (NetworkController _ p2 _ _) Player2 msg = writeChan p2 msg

handleSent :: NetworkController -> Player -> IO String
handleSent (NetworkController p1 _ _ _) Player1 = readChan p1
handleSent (NetworkController _ p2 _ _) Player2 = readChan p2

receive :: NetworkController -> IO Msg
receive (NetworkController _ _ rx _) = readChan rx

handleReceived :: NetworkController -> Player -> String -> IO ()
handleReceived (NetworkController _ _ rx _) player msg = writeChan rx (player, msg)

broadcast :: NetworkController -> String -> IO ()
broadcast (NetworkController p1 p2 _ _) msg = do
  writeChan p1 msg
  writeChan p2 msg

setReady :: NetworkController -> Player -> Bool -> IO ()
setReady controller player value = do
  empty <- isEmptyMVar playerReadyMVar
  if empty
    then putMVar playerReadyMVar value
    else do { swapMVar playerReadyMVar value; return () }
  return ()
  where playerReadyMVar = (ready $ getPlayerState player $ state $ controller)

start :: PortNumber -> IO NetworkController
start port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port 0)
  listen sock 2
  print $ "Listening on port " ++ show port
  controller <- newNetworkController
  forkIO (mainLoop sock controller)
  return controller

waitForPlayers :: NetworkController -> IO ()
waitForPlayers controller = do
  ready1 <- takeMVar player1ReadyMVar
  ready2 <- takeMVar player2ReadyMVar
  when ready1 (putMVar player2ReadyMVar ready1)
  when ready2 (putMVar player1ReadyMVar ready2)
  when (not ready1 || not ready2) (waitForPlayers controller)
  where player1ReadyMVar = (ready $ player1State $ state $ controller)
        player2ReadyMVar = (ready $ player2State $ state $ controller)

mainLoop :: Socket -> NetworkController -> IO ()
mainLoop sock controller = do
  (conn1, _) <- accept sock
  forkIO (runConn conn1 controller Player1)
  (conn2, _) <- accept sock
  forkIO (runConn conn2 controller Player2)
  acceptAndClose sock

acceptAndClose :: Socket -> IO () 
acceptAndClose sock = do
  (conn, _) <- accept sock
  hdl <- socketToHandle conn ReadWriteMode
  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "Server full"
  hClose hdl
  close conn
  acceptAndClose sock

runConn :: Socket -> NetworkController -> Player -> IO ()
runConn sock controller player = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    setReady controller player True

    -- fork off a thread for reading from the transmission channel and forwarding
    writer <- forkIO $ fix $ \loop -> do
        msg <- handleSent controller player
        hPutStrLn hdl msg
        loop

    -- Read and put everything in the received channel
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        msg <- fmap init (hGetLine hdl)
        case msg of
             -- If an exception is caught, send a message and break the loop
             "quit" -> hPutStrLn hdl "Bye!"
             -- else, continue looping.
             _      -> handleReceived controller player msg >> loop

    setReady controller player False
    killThread writer                      -- kill after the loop ends
    hClose hdl                             -- close the handle