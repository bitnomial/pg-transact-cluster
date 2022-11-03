{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Database.PostgreSQL.Transact.Cluster
--
-- This module gives developers a way to work with readonly queries.  The module
-- does not do any SQL analysis.  Instead, use it by explicitly tagging some
-- queries as readonly.
module Database.PostgreSQL.Transact.Cluster (
    -- * Connection
    ClusterConnPool,
    readPool,
    writePool,
    newClusterConnPool,
    newReadOnlyClusterConnPool,
    asReadOnlyPool,

    -- * Queries
    QueryMode (..),
    CDBT,
    getDBT,
    CDB,
    ExecutionMode (..),
    readonly,
    asReadWrite,
    readWrite,

    -- * Exceptions
    ClusterConnPoolException (..),
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Pool (withResource)
import Database.PostgreSQL.Transact (DBT, runDBTNoTransaction, runDBTSerializable)
import Database.PostgreSQL.Transact.Cluster.Connection (
    ClusterConnPool (..),
    ClusterConnPoolException (..),
    QueryMode (..),
    asReadOnlyPool,
    newClusterConnPool,
    newReadOnlyClusterConnPool,
    readPool,
    writePool,
 )


-- | This abstraction gives developers a way to mark 'DBT' code as read only.
newtype CDBT (mode :: QueryMode) m a = CDBT {unCDBT :: DBT m a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , Semigroup
        , Monoid
        )


getDBT :: CDBT mode m a -> DBT m a
getDBT = unCDBT


type CDB mode = CDBT mode IO


instance MonadTrans (CDBT mode) where
    lift = CDBT . lift


-- | This typeclass allows programmers to write 'QueryMode' agnostic query execution code
class ExecutionMode modeConn modeQuery where
    runSerializable ::
        MonadBaseControl IO m =>
        ClusterConnPool modeConn ->
        CDBT modeQuery m a ->
        m a
    runNoTransaction ::
        MonadBaseControl IO m =>
        ClusterConnPool modeConn ->
        CDBT modeQuery m a ->
        m a


instance ExecutionMode 'ReadOnly 'ReadOnly where
    runSerializable ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTSerializable task)
    runNoTransaction ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTNoTransaction task)


instance ExecutionMode 'ReadWrite 'ReadOnly where
    runSerializable ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTSerializable task)
    runNoTransaction ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTNoTransaction task)


instance ExecutionMode 'ReadWrite 'ReadWrite where
    runSerializable ClusterConnPool{writeReplicaConns} (CDBT task) =
        control $ \run ->
            withResource writeReplicaConns (run . runDBTSerializable task)
    runNoTransaction ClusterConnPool{writeReplicaConns} (CDBT task) =
        control $ \run ->
            withResource writeReplicaConns (run . runDBTNoTransaction task)


-- | Mark a query as readonly.  Note that the result is polymorphic, so
-- developers can use it in either a 'ReadOnly' or a 'ReadWrite' context
readonly :: forall mode a m. DBT m a -> CDBT mode m a
readonly = CDBT


-- | Mark any query as a 'ReadWrite'.  This is useful for including 'ReadOnly'
-- code in a 'ReadWrite' value
asReadWrite :: CDBT mode m a -> CDBT 'ReadWrite m a
asReadWrite = CDBT . unCDBT


-- | Lift an arbitrary query
readWrite :: DBT m a -> CDBT 'ReadWrite m a
readWrite = CDBT
