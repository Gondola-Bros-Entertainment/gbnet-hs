{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Net
-- Description : NetT monad transformer for network operations
--
-- Provides a monad transformer that carries network state (socket, local address)
-- and implements MonadNetwork. This allows polymorphic network code that can
-- run against real IO or pure test backends.
module GBNet.Net
  ( -- * Network state
    NetState (..),
    newNetState,

    -- * NetT monad transformer
    NetT (..),
    runNetT,
    evalNetT,
    execNetT,

    -- * State access
    getNetState,
    getsNetState,
    modifyNetState,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (StateT (..), evalStateT, execStateT, get, gets, modify')
import GBNet.Class (MonadNetwork (..), MonadTime (..), NetError (..), getMonoTimeIO)
import GBNet.Security (validateAndStripCrc32)
import GBNet.Socket
  ( SocketError (..),
    UdpSocket,
    closeSocket,
    socketRecvFrom,
    socketSendTo,
  )
import Network.Socket (SockAddr)

-- | Network state carried by NetT.
data NetState = NetState
  { -- | The UDP socket
    nsSocket :: !UdpSocket,
    -- | Local address this peer is bound to
    nsLocalAddr :: !SockAddr
  }

-- | Create initial network state from a socket.
newNetState :: UdpSocket -> SockAddr -> NetState
newNetState sock addr =
  NetState
    { nsSocket = sock,
      nsLocalAddr = addr
    }

-- | Network monad transformer.
--
-- Wraps StateT to carry network state. When @m@ is IO, this provides real
-- UDP networking via the 'MonadNetwork' instance defined below.
-- For testing, use 'TestNet' from "GBNet.TestNet" instead.
newtype NetT m a = NetT {unNetT :: StateT NetState m a}
  deriving (Functor, Applicative, Monad)

-- | Run a NetT computation with initial state, returning result and final state.
runNetT :: NetT m a -> NetState -> m (a, NetState)
runNetT (NetT m) = runStateT m

-- | Run a NetT computation, discarding final state.
evalNetT :: (Monad m) => NetT m a -> NetState -> m a
evalNetT (NetT m) = evalStateT m

-- | Run a NetT computation, returning only final state.
execNetT :: (Monad m) => NetT m a -> NetState -> m NetState
execNetT (NetT m) = execStateT m

-- | Get the current network state.
getNetState :: (Monad m) => NetT m NetState
getNetState = NetT get

-- | Get a field from network state.
getsNetState :: (Monad m) => (NetState -> a) -> NetT m a
getsNetState = NetT . gets

-- | Modify network state.
modifyNetState :: (Monad m) => (NetState -> NetState) -> NetT m ()
modifyNetState = NetT . modify'

-- | MonadIO instance for NetT.
instance (MonadIO m) => MonadIO (NetT m) where
  liftIO = NetT . liftIO

-- | MonadTime instance for NetT IO.
instance MonadTime (NetT IO) where
  getMonoTime = liftIO getMonoTimeIO

-- | MonadNetwork instance for NetT IO.
--
-- Provides real UDP networking through the socket in NetState.
instance MonadNetwork (NetT IO) where
  netSend toAddr bytes = do
    st <- getNetState
    now <- liftIO getMonoTimeIO
    result <- liftIO $ socketSendTo bytes toAddr now (nsSocket st)
    case result of
      Left err -> pure $ Left (NetSendFailed (show err))
      Right (_, sock') -> do
        modifyNetState $ \s -> s {nsSocket = sock'}
        pure $ Right ()

  netRecv = do
    st <- getNetState
    now <- liftIO getMonoTimeIO
    result <- liftIO $ socketRecvFrom now (nsSocket st)
    case result of
      Left SocketWouldBlock -> pure Nothing
      Left _ -> pure Nothing
      Right (dat, addr, sock') -> do
        modifyNetState $ \s -> s {nsSocket = sock'}
        -- Validate CRC before returning
        case validateAndStripCrc32 dat of
          Nothing -> pure Nothing -- Invalid CRC, skip
          Just validated -> pure $ Just (validated, addr)

  netClose = do
    st <- getNetState
    liftIO $ closeSocket (nsSocket st)
