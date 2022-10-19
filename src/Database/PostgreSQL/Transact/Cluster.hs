{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneKindSignatures #-}

-- |
-- Module: Database.PostgreSQL.Transact.Cluster
--
-- This module gives developers a way to work with readonly queries.  The module
-- does not do any SQL analysis.  Instead, use it by explicitly tagging some
-- queries as readonly.
module Database.PostgreSQL.Transact.Cluster (
    -- * Connection
    ClusterConnPool,
    newClusterConnPool,
    newReadOnlyClusterConnPool,

    -- * Queries
    QueryMode (..),
    CDBT,
    CDB,
    ExecutionMode (..),
    readonly,
    toReadWrite,
    liftQuery,
) where

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Pool (withResource)
import Database.PostgreSQL.Transact (DBT, runDBTNoTransaction, runDBTSerializable)
import Database.PostgreSQL.Transact.Cluster.Connection (
    ClusterConnPool (..),
    newClusterConnPool,
    newReadOnlyClusterConnPool,
 )
import Data.Kind (Constraint)


data QueryMode = ReadOnly | ReadWrite


-- | This abstraction gives developers a way to mark 'DBT' code as read only.
newtype CDBT (mode :: QueryMode) m a = CDBT {unCDBT :: DBT m a}
    deriving
        ( Functor
        , Applicative
        , Monad
        )


type CDB mode = CDBT mode IO


instance MonadTrans (CDBT mode) where
    lift = CDBT . lift


-- | This typeclass allows programmers to write 'QueryMode' agnostic query execution code
type ExecutionMode :: QueryMode -> Constraint
class ExecutionMode mode where
    runSerializable ::
        MonadBaseControl IO m =>
        ClusterConnPool ->
        CDBT mode m a ->
        m a
    runNoTransaction ::
        MonadBaseControl IO m =>
        ClusterConnPool ->
        CDBT mode m a ->
        m a


instance ExecutionMode 'ReadOnly where
    runSerializable ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTSerializable task)
    runNoTransaction ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTNoTransaction task)


instance ExecutionMode 'ReadWrite where
    runSerializable ClusterConnPool{writeReplicaConns} (CDBT task) =
        control $ \run ->
            withResource writeReplicaConns (run . runDBTSerializable task)
    runNoTransaction ClusterConnPool{writeReplicaConns} (CDBT task) =
        control $ \run ->
            withResource writeReplicaConns (run . runDBTNoTransaction task)


-- | Mark a query as readonly
readonly :: DBT m a -> CDBT 'ReadOnly m a
-- It almost feels like this should be called 'unsafeReadOnly' (unless we also
-- want to provide a wrapper around DBT that only allows the 'query' function
-- to be used (not 'execute')). The scope of pg-transact is pretty small, so
-- this might not be a crazy idea.
readonly = CDBT


-- | Mark any query as a 'ReadWrite'.  This is useful for including 'ReadOnly'
-- code in a 'ReadWrite' value
toReadWrite :: CDBT mode m a -> CDBT 'ReadWrite m a
toReadWrite = CDBT . unCDBT


-- | Lift an arbitrary query
liftQuery :: DBT m a -> CDBT 'ReadWrite m a
liftQuery = CDBT
