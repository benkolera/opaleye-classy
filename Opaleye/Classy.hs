{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Opaleye.Classy
  ( OpaleyeEnv(OpaleyeEnv,_opaleyeEnvConn)
  , opaleyeEnvConn
  , closeEnv
  , OpaleyeError(OpaleyeSqlError,OpaleyeQueryError,OpaleyeResultError)
  , _OpaleyeSqlError, _OpaleyeQueryError, _OpaleyeResultError
  , CanOpaleye
  , liftQueryFirst
  , liftQuery
  , liftInsert
  , liftInsertReturning
  , liftDelete
  , liftUpdate
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

data OpaleyeEnv = OpaleyeEnv
  { _opaleyeEnvConn :: Connection }
makeClassy ''OpaleyeEnv

data OpaleyeError
  = OpaleyeSqlError SqlError
  | OpaleyeQueryError QueryError
  | OpaleyeResultError ResultError
  deriving Show
makeClassyPrisms ''OpaleyeError

type CanOpaleye m c e =
  ( MonadReader c m
  , MonadError e m
  , MonadIO m
  , Applicative m
  , AsOpaleyeError e
  , HasOpaleyeEnv c
  )

liftQueryFirst
  :: ( CanOpaleye m c e
    , Default QueryRunner a b
    , Applicative m
    )
  => Query a
 -> m (Maybe b)
liftQueryFirst = fmap (^? _head) . liftQuery

liftQuery
  :: ( CanOpaleye m c e
    , Default QueryRunner a b
    )
  => Query a
  -> m [b]
liftQuery q = withConn (`runQuery` q)

liftInsert
  :: CanOpaleye m c e
  => Table colW colR
  -> colW
  -> m Int64
liftInsert t w = withConn $ \conn -> runInsert conn t w

liftInsertReturning
  :: ( CanOpaleye m c e
    , Default QueryRunner ret hask
    , Default Unpackspec ret ret
    )
  => Table colW colR
  -> (colR -> ret)
  -> colW
  -> m [hask]
liftInsertReturning t f c = withConn $ \conn -> runInsertReturning conn t c f

liftUpdate
  :: CanOpaleye m c e
  => Table colW colR
  -> (colR -> colW)
  -> (colR -> Column PGBool)
  -> m Int64
liftUpdate t u p = withConn $ \conn -> runUpdate conn t u p

liftDelete
  :: CanOpaleye m c e
  => Table colW colR
  -> (colR -> Column PGBool)
  -> m Int64
liftDelete t p = withConn $ \conn -> runDelete conn t p

withConn :: CanOpaleye m c e => (Connection -> IO a) -> m a
withConn f = view opaleyeEnvConn >>= flip withGivenConn f

withGivenConn :: CanOpaleye m c e => Connection -> (Connection -> IO a) -> m a
withGivenConn c f = liftIO $ f c

closeEnv :: OpaleyeEnv -> IO ()
closeEnv = close . (^.opaleyeEnvConn)

derivePGField
  :: forall a b. FromField a
  => (a -> b)
  -> Field
  -> Maybe ByteString
  -> Conversion b
derivePGField c f = fmap c . fromField f
