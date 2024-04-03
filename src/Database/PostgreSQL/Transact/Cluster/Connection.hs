{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Pool (Pool, defaultPoolConfig, newPool)
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


-- | Effectively a wrapper around 'defaultPoolConfig' which makes it easy to
-- automatically create 'Connection's from multiple 'IO' actions.
--
-- When the 'Pool' goes to create a new 'Connection', it rotates through the
-- passed-in input actions, using each one once to create a new 'Connection'.
-- After using up all input actions, it starts over back at the beginning of
-- the list.
--
-- Used like the following:
--
-- @
--   connPool
--     [ connectPostgreSQL "postgresql://my-user:my-pass@host1:5432/my-db"
--     , connectPostgreSQL "postgresql://my-user:my-pass@host2:5432/my-db"
--     , connectPostgreSQL "postgresql://my-user:my-pass@host3:5432/my-db"
--     ]
-- @
connPool :: [IO Connection] -> IO (Pool Connection)
connPool = connPoolGeneric close


-- | A generic version of 'connPool'.
--
-- This is useful for testing in GHCi.
connPoolGeneric ::
    forall a.
    -- | close action
    (a -> IO ()) ->
    [IO a] ->
    IO (Pool a)
connPoolGeneric closeConn conns = do
    maxResources <- (* 2) <$> getNumCapabilities
    stripedIOs <- stripeActions conns
    newPool $ defaultPoolConfig stripedIOs closeConn idleTime maxResources
  where
    idleTime = 10


-- | Stripe a set of 'IO' actions.
--
-- Takes a list of 'IO' actions, and returns a wrapped 'IO' action.  The inner 'IO'
-- action will iterate through the passed-in actions, running the next one
-- every time it is called.
--
-- >>> foo <- stripeActions [putStrLn "hello" *> pure 1, putStrLn "bye" *> pure 2, putStrLn "goat" *> pure 3] :: IO (IO Int)
-- >>> foo
-- hello
-- 1
-- >>> foo
-- bye
-- 2
-- >>> foo
-- goat
-- 3
-- >>> foo
-- hello
-- 1
-- >>> foo
-- bye
-- 2
--
-- This is similar to a function like
-- @'sequence' . 'cycle' :: ['IO' a] -> 'IO' [a]@, but instead of returning a
-- list of values, it returns an 'IO' action that will always run the next
-- action from the input list.
stripeActions :: [IO a] -> IO (IO a)
stripeActions [] = throwIO NoConnection
stripeActions (ioConn : moreConns) = do
    -- This IORef keeps track of how many of the input items we've gone through
    -- so far.
    refConnectors <- newIORef (ioConn : moreConns)
    pure $
        -- This is the heart of this function. It first modifies the IORef:
        --
        -- 1. If there are still values remaining in the IORef, return the first
        --    value from the IORef, and set the IORef to the remaining values.
        -- 2. If there are no more remaining values in the IORef, return the
        --    first value passed in to stripeActions.  Set the IORef to the
        --    remaining values.
        --
        -- The join function runs the IO action returned from atomicModifyIORef'.
        join $
            atomicModifyIORef' refConnectors $
                \case
                    x : xs -> (xs, x)
                    [] -> (moreConns, ioConn)


data ClusterConnPoolException = NoConnection
    deriving (Eq, Show)


instance Exception ClusterConnPoolException
