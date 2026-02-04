-- |
-- Module      : GBNet.Net.IO
-- Description : IO-based network initialization
--
-- Provides initialization helpers for creating NetState with real sockets.
module GBNet.Net.IO
  ( -- * Socket operations
    initNetState,
  )
where

import GBNet.Net (NetState, newNetState)
import GBNet.Socket
  ( SocketError (..),
    closeSocket,
    newUdpSocket,
    socketLocalAddr,
  )
import Network.Socket (SockAddr)

-- | Initialize NetState by creating a socket bound to the given address.
-- Returns the socket and local address for constructing NetState.
initNetState :: SockAddr -> IO (Either SocketError NetState)
initNetState bindAddr = do
  socketResult <- newUdpSocket bindAddr
  case socketResult of
    Left err -> pure $ Left err
    Right sock -> do
      addrResult <- socketLocalAddr sock
      case addrResult of
        Left err -> do
          closeSocket sock
          pure $ Left err
        Right localAddr -> Right <$> newNetState sock localAddr
