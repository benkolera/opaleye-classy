{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Opaleye.Classy
  ( DbEnv(DbEnv,_dbEnvConn)
  , dbEnvConn
  , DbError(DbSqlError,DbQueryError,DbResultError)
  , _DbSqlError, _DbQueryError, _DbResultError
  , CanDb
  , liftQueryFirst
  , liftQuery
  , liftInsert
  , liftInsertReturning
  , liftDelete
  , liftUpdate
  , closeEnv
  , derivePGField
  ) where

import Control.Applicative                  (Applicative)
import Control.Lens
import Control.Monad.Except                 (MonadError)
import Control.Monad.Reader                 (MonadReader)
import Control.Monad.Trans                  (MonadIO, liftIO)
import Data.ByteString                      (ByteString)
import Data.Int                             (Int64)
import Data.Profunctor.Product.Default      (Default)
import Database.PostgreSQL.Simple           (Connection, QueryError, ResultError, SqlError, close)
import Database.PostgreSQL.Simple.FromField (Conversion, Field, FromField (fromField))
import Opaleye
  ( Column, PGBool, Query, QueryRunner, Table, Unpackspec, runDelete, runInsert
  , runInsertReturning, runQuery, runUpdate
  )

data DbEnv = DbEnv
  { _dbEnvConn :: Connection }
makeClassy ''DbEnv

data DbError
  = DbSqlError SqlError
  | DbQueryError QueryError
  | DbResultError ResultError
  deriving Show
makeClassyPrisms ''DbError

type CanDb m c e =
  ( MonadReader c m
  , MonadError e m
  , MonadIO m
  , Applicative m
  , AsDbError e
  , HasDbEnv c
  )

liftQueryFirst
  :: ( CanDb m c e
    , Default QueryRunner a b
    , Applicative m
    )
  => Query a
 -> m (Maybe b)
liftQueryFirst = fmap (^? _head) . liftQuery

liftQuery
  :: ( CanDb m c e
    , Default QueryRunner a b
    )
  => Query a
  -> m [b]
liftQuery q = withConn (`runQuery` q)

liftInsert
  :: CanDb m c e
  => Table colW colR
  -> colW
  -> m Int64
liftInsert t w = withConn $ \conn -> runInsert conn t w

liftInsertReturning
  :: ( CanDb m c e
    , Default QueryRunner ret hask
    , Default Unpackspec ret ret
    )
  => Table colW colR
  -> (colR -> ret)
  -> colW
  -> m [hask]
liftInsertReturning t f c = withConn $ \conn -> runInsertReturning conn t c f

liftUpdate
  :: CanDb m c e
  => Table colW colR
  -> (colR -> colW)
  -> (colR -> Column PGBool)
  -> m Int64
liftUpdate t u p = withConn $ \conn -> runUpdate conn t u p

liftDelete
  :: CanDb m c e
  => Table colW colR
  -> (colR -> Column PGBool)
  -> m Int64
liftDelete t p = withConn $ \conn -> runDelete conn t p

withConn :: CanDb m c e => (Connection -> IO a) -> m a
withConn f = view dbEnvConn >>= flip withGivenConn f

withGivenConn :: CanDb m c e => Connection -> (Connection -> IO a) -> m a
withGivenConn c f = liftIO $ f c

closeEnv :: DbEnv -> IO ()
closeEnv = close . (^.dbEnvConn)

derivePGField
  :: forall a b. FromField a
  => (a -> b)
  -> Field
  -> Maybe ByteString
  -> Conversion b
derivePGField c f = fmap c . fromField f
