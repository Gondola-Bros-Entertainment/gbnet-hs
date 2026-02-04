{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : GBNet.Class
-- Description : Effect abstractions for networking
--
-- Typeclasses that abstract over network IO, enabling:
-- - Pure testing with deterministic "networks"
-- - Swappable backends (real sockets, mock, simulation)
-- - Clean separation of pure logic from IO
module GBNet.Class
  ( -- * Time
    MonoTime (..),
    MonadTime (..),
    getMonoTimeIO,

    -- * Network IO
    MonadNetwork (..),
    NetError (..),
  )
where

import Control.Monad.State.Strict (StateT (..))
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.Socket (SockAddr)

-- | Monotonic time in nanoseconds.
-- Derives 'Num' because arithmetic on timestamps is pervasive.
newtype MonoTime = MonoTime {unMonoTime :: Word64}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Bounded, Enum, Real, Integral)

-- | Network errors.
data NetError
  = NetSendFailed !String
  | NetSocketClosed
  | NetTimeout
  deriving (Eq, Show)

-- | Monad that can provide monotonic time.
class (Monad m) => MonadTime m where
  -- | Get current monotonic time in nanoseconds.
  getMonoTime :: m MonoTime

-- | Monad that can perform network IO.
--
-- This abstraction allows:
-- - Real IO with UDP sockets
-- - Pure testing with simulated networks
-- - Network condition simulation (latency, loss, reordering)
class (MonadTime m) => MonadNetwork m where
  -- | Send raw bytes to an address.
  -- Returns Left on failure, Right () on success.
  netSend :: SockAddr -> ByteString -> m (Either NetError ())

  -- | Receive bytes (non-blocking).
  -- Returns Nothing if no data available, Just (data, sender) otherwise.
  netRecv :: m (Maybe (ByteString, SockAddr))

  -- | Close the network (cleanup).
  netClose :: m ()

-- | Lift MonadTime through StateT.
instance (MonadTime m) => MonadTime (StateT s m) where
  getMonoTime = lift getMonoTime

-- | Lift MonadNetwork through StateT.
instance (MonadNetwork m) => MonadNetwork (StateT s m) where
  netSend addr bs = lift (netSend addr bs)
  netRecv = lift netRecv
  netClose = lift netClose

-- | Get current monotonic time in nanoseconds (IO helper).
getMonoTimeIO :: IO MonoTime
getMonoTimeIO = MonoTime <$> getMonotonicTimeNSec

-- | MonadTime instance for IO.
instance MonadTime IO where
  getMonoTime = getMonoTimeIO
