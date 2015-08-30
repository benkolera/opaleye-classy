{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
module Opaleye.Classy
  ( OpaleyeEnv(OpaleyeEnv,_opaleyeEnvConn)
  , HasOpaleyeEnv(opaleyeEnv,opaleyeEnvConn)
  , closeEnv
  , OpaleyeError(OpaleyeSqlError,OpaleyeQueryError,OpaleyeResultError)
  , AsOpaleyeError(_OpaleyeError,_OpaleyeSqlError, _OpaleyeQueryError, _OpaleyeResultError)
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

type CanOpaleye c e m =
  ( MonadReader c m
  , MonadError e m
  , MonadIO m
  -- This applicative constraint doesn't actually need to be here,
  -- but it makes the constraint easier to deal with in base < 4.8
  -- I'm personally stuck on GHC 7.8 until we can upgrade to
  -- Centos 7, so this is very helpful for me. :)
  , Applicative m
  , AsOpaleyeError e
  , HasOpaleyeEnv c
  )

liftQueryFirst
  :: ( CanOpaleye c e m
    , Default QueryRunner a b
    , Applicative m
    )
  => Query a
 -> m (Maybe b)
liftQueryFirst = fmap (^? _head) . liftQuery

liftQuery
  :: ( CanOpaleye c e m
    , Default QueryRunner a b
    )
  => Query a
  -> m [b]
liftQuery q = withConn (`runQuery` q)

liftInsert
  :: CanOpaleye c e m
  => Table colW colR
  -> colW
  -> m Int64
liftInsert t w = withConn $ \conn -> runInsert conn t w

liftInsertReturning
  :: ( CanOpaleye c e m
    , Default QueryRunner ret hask
    , Default Unpackspec ret ret
    )
  => Table colW colR
  -> (colR -> ret)
  -> colW
  -> m [hask]
liftInsertReturning t f c = withConn $ \conn -> runInsertReturning conn t c f

liftUpdate
  :: CanOpaleye c e m
  => Table colW colR
  -> (colR -> colW)
  -> (colR -> Column PGBool)
  -> m Int64
liftUpdate t u p = withConn $ \conn -> runUpdate conn t u p

liftDelete
  :: CanOpaleye c e m
  => Table colW colR
  -> (colR -> Column PGBool)
  -> m Int64
liftDelete t p = withConn $ \conn -> runDelete conn t p

withConn :: CanOpaleye c e m => (Connection -> IO a) -> m a
withConn f = view opaleyeEnvConn >>= flip withGivenConn f

withGivenConn :: CanOpaleye c e m => Connection -> (Connection -> IO a) -> m a
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
