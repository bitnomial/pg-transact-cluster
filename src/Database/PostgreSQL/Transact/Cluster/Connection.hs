{-# LANGUAGE RankNTypes #-}
-- For compatibility with resource-pool version 0.2.*
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |
-- Module: Database.PostgreSQL.Transact.Cluster.Connection
--
-- This module defines a connection type which mediates access to a postgres
-- cluster with both read and write endpoints
module Database.PostgreSQL.Transact.Cluster.Connection (
    ClusterConnPool (..),
    newClusterConnPool,
    newReadOnlyClusterConnPool,
) where

import Control.Concurrent (getNumCapabilities)
import Control.Monad (join)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Pool (Pool, createPool)
import Database.PostgreSQL.Simple (Connection, close)
import GHC.IORef (IORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty


-- I haven't really thought through this, but maybe you could tag
-- 'ClusterConnPool' with 'QueryMode' as well?  That way you might be able to avoid
-- having 'newReadOnlyClusterConnPool' that fails at runtime if you try to execute Write queries.
data ClusterConnPool = ClusterConnPool
    { readReplicaConns :: Pool Connection
    , writeReplicaConns :: Pool Connection
    }


-- | If either list of connectors is empty, then an attempt to run a query
-- against that style of endpoint will raise 'ClusterConnPoolException'
newClusterConnPool ::
    -- | Read replica connection creators
    NonEmpty (IO Connection) ->
    -- | Write replica connection creators
    NonEmpty (IO Connection) ->
    IO ClusterConnPool
newClusterConnPool readConnectors writeConnectors =
    ClusterConnPool
        <$> connPool readConnectors
        <*> connPool writeConnectors


-- | Configure a read-only pool.  Any attempt to make a 'ReadWrite' query will
-- fail with 'ClusterConnPoolException'.  If the list is empty, then any attempt
-- to run a query will raise 'ClusterConnPoolException'
newReadOnlyClusterConnPool :: NonEmpty (IO Connection) -> IO ClusterConnPool
newReadOnlyClusterConnPool readConnectors =
    -- TODO: Don't use undefined here.
    newClusterConnPool readConnectors undefined


-- I stared at this for quite a few minutes, and I couldn't wrap my head around what it does.
-- Squinting a little bit, it looks like it loops over the @[IO Connection]@ passed in as input,
-- and returns a new one each time.  Then it starts over when it has gotten to the end of the list.
--
-- A couple questions I had about it that I couldn't figure out are:
--
-- 1. Why does it take a @[IO Connection]@ and not just @IO Connection@?  Do we
--    use this list-of-IO-Connection somewhere?
--
-- 2. It looks like the type of @setup@ is
--    @[IO Connection] -> IO (IO (IO Connection))@.  What do each level of the
--    result @IO@s semantically mean?
-- connPool :: [IO Connection] -> IO (Pool Connection)
-- connPool connectors = do
--     getConnector <- setup connectors
--     createPool
--         (join getConnector)
--         close
--         5
--         10
--         =<< getNumCapabilities
--   where
--     setup = \case
--         c : cs -> do
--             refConnectors <- newIORef connectors
--             pure . atomicModifyIORef' refConnectors
--                 $ \case
--                     x : xs -> (xs, x)
--                     _ -> (cs, c)
--         _ -> pure $ throwIO NoConnection




-- _edit_: After looking at the above function for a while, I guess I sorta
-- figured it out.  You can ignore my questions above.
--
-- I've tried to simplify it here a little, but I'm not quite
-- sure I was able to keep the same semantics.
connPool :: NonEmpty (IO Connection) -> IO (Pool Connection)
connPool connectors = do
    -- TODO: Instead of using NonEmpty.cycle here, it would be better if we had a function like
    -- @Stream.cycle :: NonEmpty a -> Stream a@ that denoted an infinite list.
    -- Then we wouldn't have to handle the error in getConnection.
    refConnectors <- newIORef $ NonEmpty.cycle connectors
    numCaps <- getNumCapabilities
    createPool (getConnection refConnectors) close 5 10 numCaps
  where
    getConnection :: IORef (NonEmpty (IO Connection)) -> IO Connection
    getConnection refConnectors = join $ atomicModifyIORef' refConnectors modFunc

    modFunc :: NonEmpty (IO Connection) -> (NonEmpty (IO Connection), IO Connection)
    modFunc (connector :| connectors') =
        case connectors' of
            [] -> error "connPool': The logic in this function is setup to never run out of connections"
            connHead : connTail -> (connHead :| connTail, connector)
