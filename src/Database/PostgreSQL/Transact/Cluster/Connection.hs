{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
-- For compatibility with resource-pool version 0.2.*
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module: Database.PostgreSQL.Transact.Cluster.Connection
--
-- This module defines a connection type which mediates access to a postgres
-- cluster with both read and write endpoints
module Database.PostgreSQL.Transact.Cluster.Connection (
    QueryMode (..),
    ClusterConnPool (..),
    readPool,
    writePool,
    newClusterConnPool,
    newReadOnlyClusterConnPool,
    asReadOnlyPool,
    ClusterConnPoolException (..),
) where

import Control.Concurrent (getNumCapabilities)
import Control.Exception (Exception, throwIO)
import Control.Monad (join)
import Data.Coerce (coerce)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close)


data QueryMode = ReadOnly | ReadWrite


data ClusterConnPool (mode :: QueryMode) = ClusterConnPool
    { readReplicaConns :: Pool Connection
    , writeReplicaConns :: Pool Connection
    }


readPool :: ClusterConnPool mode -> Pool Connection
readPool = readReplicaConns


writePool :: ClusterConnPool 'ReadWrite -> Pool Connection
writePool = writeReplicaConns


-- | If either list of connectors is empty, then an attempt to run a query
-- against that style of endpoint will raise 'ClusterConnPoolException'
newClusterConnPool ::
    -- | Read replica connection creators
    [IO Connection] ->
    -- | Write replica connection creators
    [IO Connection] ->
    IO (ClusterConnPool mode)
newClusterConnPool readConnectors writeConnectors =
    ClusterConnPool
        <$> connPool readConnectors
        <*> connPool writeConnectors


-- | Configure a read-only pool.  Any attempt to make a 'ReadWrite' query will
-- fail with 'ClusterConnPoolException'.  If the list is empty, then any attempt
-- to run a query will raise 'ClusterConnPoolException'
newReadOnlyClusterConnPool :: [IO Connection] -> IO (ClusterConnPool 'ReadOnly)
newReadOnlyClusterConnPool readConnectors =
    newClusterConnPool readConnectors mempty


-- | Drop the type-level write capability on a pool
asReadOnlyPool :: ClusterConnPool 'ReadWrite -> ClusterConnPool 'ReadOnly
asReadOnlyPool = coerce


connPool :: [IO Connection] -> IO (Pool Connection)
connPool connectors = do
    getConnector <- setup connectors
    createPool
        (join getConnector)
        close
        5
        10
        =<< getNumCapabilities
  where
    setup = \case
        c : cs -> do
            refConnectors <- newIORef connectors
            pure
                . atomicModifyIORef' refConnectors
                $ \case
                    x : xs -> (xs, x)
                    _ -> (cs, c)
        _ -> pure $ throwIO NoConnection


data ClusterConnPoolException = NoConnection
    deriving (Eq, Show)


instance Exception ClusterConnPoolException
