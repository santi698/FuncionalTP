module NetworkController (
  start,
  broadcast,
  send,
  receive,
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
  BufferMode(NoBuffering))
import Control.Exception
import Control.Concurrent (newChan, readChan, writeChan, dupChan, forkIO, killThread, Chan)
import Control.Monad (when)
import Control.Monad.Fix (fix)

main = do 
  controller <- start
  fix $ \loop -> do
    send controller Player1 "Welcome, you are player 1"
    send controller Player2 "Welcome, you are player 2"
    broadcast controller "All players ready"
    msg <- receive controller
    print msg
    loop

data Player = Player1 | Player2 deriving (Show)

type Msg = (Player, String)

data NetworkController = NetworkController {
  player1 :: Chan String,
  player2 :: Chan String,
  received :: Chan Msg
}

newNetworkController :: IO NetworkController
newNetworkController = do
  player1Chan <- newChan
  player2Chan <- newChan
  receiveChan <- newChan
  return $ NetworkController player1Chan player2Chan receiveChan

send :: NetworkController -> Player -> String -> IO ()
send (NetworkController p1 _ _) Player1 msg = writeChan p1 msg
send (NetworkController _ p2 _) Player2 msg = writeChan p2 msg

handleSent :: NetworkController -> Player -> IO String
handleSent (NetworkController p1 _ _) Player1 = readChan p1
handleSent (NetworkController _ p2 _) Player2 = readChan p2

receive :: NetworkController -> IO Msg
receive (NetworkController _ _ rx) = readChan rx

handleReceived :: NetworkController -> Player -> String -> IO ()
handleReceived (NetworkController _ _ rx) player msg = writeChan rx (player, msg)

broadcast :: NetworkController -> String -> IO ()
broadcast (NetworkController p1 p2 _) msg = do
  writeChan p1 msg
  writeChan p2 msg

start :: IO NetworkController
start = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 0)
  listen sock 2
  print "Listening on port 4242"
  controller <- newNetworkController
  forkIO (mainLoop sock controller)
  return controller

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

    hPutStrLn hdl "Hi, what's your name?"
    name <- fmap init (hGetLine hdl)
    broadcast controller ("--> " ++ name ++ " entered game.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!")

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

    killThread writer                      -- kill after the loop ends
    hClose hdl                             -- close the handle