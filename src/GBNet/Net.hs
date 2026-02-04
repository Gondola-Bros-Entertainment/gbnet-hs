{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GBNet.Net
-- Description : NetT monad transformer for network operations
--
-- Provides a monad transformer that carries network state (socket, local address)
-- and implements MonadNetwork. This allows polymorphic network code that can
-- run against real IO or pure test backends.
--
-- The IO backend uses a dedicated receive thread that blocks efficiently on
-- the socket via GHC's IO manager (epoll\/kqueue), delivering packets through
-- an STM 'TQueue' with zero polling overhead.
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

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Exception (IOException, handle)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State.Strict (StateT (..), evalStateT, execStateT, get, gets, modify')
import qualified Data.ByteString as BS
import GBNet.Class (MonadNetwork (..), MonadTime (..), NetError (..), getMonoTimeIO)
import GBNet.Security (validateAndStripCrc32)
import GBNet.Socket
  ( UdpSocket (..),
    closeSocket,
    maxUdpPacketSize,
    socketSendTo,
  )
import GBNet.Stats (SocketStats (..))
import Network.Socket (SockAddr, Socket)
import qualified Network.Socket.ByteString as NSB

-- | Network state carried by NetT.
--
-- Contains a 'TQueue' fed by a dedicated receive thread, eliminating
-- the need for polling timeouts on the socket.
data NetState = NetState
  { -- | The UDP socket (used for sending)
    nsSocket :: !UdpSocket,
    -- | Local address this peer is bound to
    nsLocalAddr :: !SockAddr,
    -- | Queue of received packets from the receive thread
    nsRecvQueue :: !(TQueue (BS.ByteString, SockAddr)),
    -- | Receive thread handle (for cleanup)
    nsRecvThread :: !ThreadId
  }

-- | Create network state and start the dedicated receive thread.
--
-- The receive thread blocks on the socket via GHC's IO manager
-- (epoll on Linux, kqueue on macOS), consuming zero CPU when idle.
-- Received packets are delivered through an STM 'TQueue'.
newNetState :: UdpSocket -> SockAddr -> IO NetState
newNetState sock addr = do
  queue <- newTQueueIO
  tid <- forkIO $ recvLoop (usSocket sock) queue
  pure
    NetState
      { nsSocket = sock,
        nsLocalAddr = addr,
        nsRecvQueue = queue,
        nsRecvThread = tid
      }

-- | Dedicated receive thread.
--
-- Blocks on 'recvFrom' which GHC's runtime handles efficiently via the
-- IO manager — the green thread is parked with no CPU cost until data
-- arrives. Exits cleanly when the socket is closed.
recvLoop :: Socket -> TQueue (BS.ByteString, SockAddr) -> IO ()
recvLoop sock queue = go
  where
    go =
      handle (\(_ :: IOException) -> pure ()) $ do
        (dat, addr) <- NSB.recvFrom sock maxUdpPacketSize
        atomically $ writeTQueue queue (dat, addr)
        go

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
-- Receives are non-blocking reads from the TQueue (fed by the dedicated
-- receive thread). Sends go directly through the socket.
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
    result <- liftIO $ atomically $ tryReadTQueue (nsRecvQueue st)
    case result of
      Nothing -> pure Nothing
      Just (dat, addr) -> do
        -- Update receive stats on the socket
        now <- liftIO getMonoTimeIO
        let len = BS.length dat
            sock = nsSocket st
            stats = usStats sock
            stats' =
              stats
                { ssBytesReceived = ssBytesReceived stats + fromIntegral len,
                  ssPacketsReceived = ssPacketsReceived stats + 1,
                  ssLastReceiveTime = Just now
                }
        modifyNetState $ \s -> s {nsSocket = (nsSocket s) {usStats = stats'}}
        -- Validate CRC before returning
        case validateAndStripCrc32 dat of
          Nothing -> do
            -- Increment CRC drop counter
            modifyNetState $ \s ->
              let sock' = nsSocket s
                  st' = (usStats sock') {ssCrcDrops = ssCrcDrops (usStats sock') + 1}
               in s {nsSocket = sock' {usStats = st'}}
            pure Nothing
          Just validated -> pure $ Just (validated, addr)

  netClose = do
    st <- getNetState
    liftIO $ killThread (nsRecvThread st)
    liftIO $ closeSocket (nsSocket st)
