{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
    CDBT (..),
    CDB,
    hoistCDBT,
    ExecutionMode (..),
    readonly,
    asReadWrite,
    readWrite,

    -- * Exceptions
    ClusterConnPoolException (..),
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl, RunInBase, StM, control)
import Control.Monad.Trans.Reader (ReaderT (ReaderT), runReaderT)
import qualified Data.ByteString as BS
import Data.Pool (withResource)
import Database.PostgreSQL.Simple (Connection, SqlError (SqlError, sqlErrorMsg))
import Database.PostgreSQL.Simple.Transaction (
    IsolationLevel (RepeatableRead),
    TransactionMode (TransactionMode),
 )
import qualified Database.PostgreSQL.Simple.Transaction as PSQL
import Database.PostgreSQL.Transact (DBT (DBT), runDBTNoTransaction, runDBTSerializable, unDBT)
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
newtype CDBT (mode :: QueryMode) m a = CDBT {getDBT :: DBT m a}
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadFail
        , Semigroup
        , Monoid
        )


hoistCDBT :: (forall x. m x -> n x) -> CDBT mode m a -> CDBT mode n a
hoistCDBT nt (CDBT (DBT (ReaderT f))) = CDBT . DBT . ReaderT $ nt . f


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


runReadOnly ::
    MonadBaseControl IO m =>
    RunInBase m IO ->
    CDBT 'ReadOnly m r ->
    Connection ->
    IO (StM m r)
runReadOnly run (CDBT task) conn =
    PSQL.withTransactionModeRetry txMode isSerializationAnomaly conn q
  where
    txMode = TransactionMode RepeatableRead PSQL.ReadOnly
    isSerializationAnomaly SqlError{sqlErrorMsg} =
        "concurrent update" `BS.isInfixOf` sqlErrorMsg
    q = run $ runReaderT (unDBT task) conn


instance ExecutionMode 'ReadOnly 'ReadOnly where
    runSerializable ClusterConnPool{readReplicaConns} task =
        control $ \run ->
            withResource readReplicaConns (runReadOnly run task)
    runNoTransaction ClusterConnPool{readReplicaConns} (CDBT task) =
        control $ \run ->
            withResource readReplicaConns (run . runDBTNoTransaction task)


instance ExecutionMode 'ReadWrite 'ReadOnly where
    runSerializable ClusterConnPool{readReplicaConns} task =
        control $ \run ->
            withResource readReplicaConns (runReadOnly run task)
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
asReadWrite = CDBT . getDBT


-- | Lift an arbitrary query
readWrite :: DBT m a -> CDBT 'ReadWrite m a
readWrite = CDBT
