{-# LANGUAGE DataKinds #-}

-- |
-- Module      : GBNet.Peer.Migration
-- Description : Pure migration detection helpers
--
-- Finds migration candidates by matching incoming packet sequence numbers
-- against existing connections. Only depends on 'GBNet.Peer.Internal'.
module GBNet.Peer.Migration
  ( findMigrationCandidate,
    migrationTokenFor,
    migrationCooldownMs,
  )
where

import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GBNet.Class (MonoTime)
import GBNet.Config (NetworkConfig (..))
import GBNet.Connection (Connection)
import qualified GBNet.Connection as Conn
import GBNet.Packet (Packet (..), PacketHeader (..))
import GBNet.Peer.Internal (NetPeer (..), PeerId)

-- | Migration cooldown in milliseconds.
migrationCooldownMs :: Double
migrationCooldownMs = 5000.0

-- | Find a connection that could match this packet (for migration).
-- Returns (oldPeerId, connection, migrationToken) if found.
findMigrationCandidate :: Packet -> MonoTime -> NetPeer -> Maybe (PeerId, Connection, Word64)
findMigrationCandidate pkt _now peer =
  findCandidate (Map.toList (npConnections peer))
  where
    incomingSeq = sequenceNum (pktHeader pkt)
    maxDistance = ncMaxSequenceDistance (npConfig peer)

    findCandidate [] = Nothing
    findCandidate ((pid, conn) : rest) =
      let remoteSeq = Conn.connRemoteSeq conn
          diff = abs (fromIntegral incomingSeq - fromIntegral remoteSeq :: Int)
       in if diff <= fromIntegral maxDistance
            then Just (pid, conn, migrationTokenFor conn)
            else findCandidate rest

-- | Generate a migration token for a connection.
-- This should be stable across the connection lifetime.
migrationTokenFor :: Connection -> Word64
migrationTokenFor = Conn.connClientSalt
