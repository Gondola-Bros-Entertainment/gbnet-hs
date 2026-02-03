-- |
-- Module      : GBNet.Socket
-- Description : Non-blocking UDP socket wrapper with statistics
--
-- Platform-agnostic non-blocking UDP socket wrapper with per-socket
-- statistics tracking.
module GBNet.Socket
  ( -- * Constants
    maxUdpPacketSize,

    -- * Socket errors
    SocketError (..),

    -- * UDP socket
    UdpSocket (..),
    newUdpSocket,
    closeSocket,
    socketLocalAddr,
    socketSendTo,
    socketRecvFrom,
    socketStats,
    socketResetStats,
  )
where

import Control.Exception (IOException, catch)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import GBNet.Reliability (MonoTime)
import GBNet.Stats (SocketStats (..), defaultSocketStats)
import Network.Socket (SockAddr, Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

-- | Maximum size of a single UDP datagram.
maxUdpPacketSize :: Int
maxUdpPacketSize = 65536

-- | Errors that can occur during socket operations.
data SocketError
  = SocketIoError !String
  | SocketInvalidAddress
  | SocketClosed
  | SocketWouldBlock
  | SocketOther !String
  deriving (Eq, Show)

-- | Non-blocking UDP socket with per-socket statistics.
data UdpSocket = UdpSocket
  { usSocket :: !Socket,
    usStats :: !SocketStats
  }
  deriving (Show)

-- | Create a new UDP socket bound to the specified address.
newUdpSocket :: SockAddr -> IO (Either SocketError UdpSocket)
newUdpSocket addr = do
  result <- tryIO $ do
    sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bind sock addr
    NS.withFdSocket sock NS.setNonBlockIfNeeded
    return sock
  return $ case result of
    Left err -> Left (SocketIoError err)
    Right sock ->
      Right
        UdpSocket
          { usSocket = sock,
            usStats = defaultSocketStats
          }

-- | Close the socket.
closeSocket :: UdpSocket -> IO ()
closeSocket sock = NS.close (usSocket sock)

-- | Get the local address this socket is bound to.
socketLocalAddr :: UdpSocket -> IO (Either SocketError SockAddr)
socketLocalAddr sock = do
  result <- tryIO $ NS.getSocketName (usSocket sock)
  return $ case result of
    Left err -> Left (SocketIoError err)
    Right addr -> Right addr

-- | Send data to a specific address.
socketSendTo ::
  BS.ByteString ->
  SockAddr ->
  MonoTime ->
  UdpSocket ->
  IO (Either SocketError (Int, UdpSocket))
socketSendTo dat addr now sock = do
  result <- tryIO $ NSB.sendTo (usSocket sock) dat addr
  return $ case result of
    Left err
      | isWouldBlock err -> Left SocketWouldBlock
      | otherwise -> Left (SocketIoError err)
    Right sent ->
      let stats = usStats sock
          stats' =
            stats
              { ssBytesSent = ssBytesSent stats + fromIntegral sent,
                ssPacketsSent = ssPacketsSent stats + 1,
                ssLastSendTime = Just now
              }
       in Right (sent, sock {usStats = stats'})

-- | Receive data from any address.
socketRecvFrom ::
  MonoTime ->
  UdpSocket ->
  IO (Either SocketError (BS.ByteString, SockAddr, UdpSocket))
socketRecvFrom now sock = do
  result <- tryIO $ NSB.recvFrom (usSocket sock) maxUdpPacketSize
  return $ case result of
    Left err
      | isWouldBlock err -> Left SocketWouldBlock
      | otherwise -> Left (SocketIoError err)
    Right (dat, addr) ->
      let len = BS.length dat
          stats = usStats sock
          stats' =
            stats
              { ssBytesReceived = ssBytesReceived stats + fromIntegral len,
                ssPacketsReceived = ssPacketsReceived stats + 1,
                ssLastReceiveTime = Just now
              }
       in Right (dat, addr, sock {usStats = stats'})

-- | Get socket statistics.
socketStats :: UdpSocket -> SocketStats
socketStats = usStats

-- | Reset socket statistics.
socketResetStats :: UdpSocket -> UdpSocket
socketResetStats sock = sock {usStats = defaultSocketStats}

-- | Try an IO action, catching IOExceptions.
tryIO :: IO a -> IO (Either String a)
tryIO action =
  (Right <$> action) `catch` handler
  where
    handler :: IOException -> IO (Either String a)
    handler e = return $ Left (show e)

-- | Check if error message indicates would-block.
isWouldBlock :: String -> Bool
isWouldBlock err =
  "resource exhausted" `isInfixOf` err
    || "would block" `isInfixOf` err
    || "EAGAIN" `isInfixOf` err
    || "EWOULDBLOCK" `isInfixOf` err
