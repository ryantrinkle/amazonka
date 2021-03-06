{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The core module for making requests to the various AWS services.
module Network.AWS
    (
    -- * Requests
    -- ** Synchronous
      send
    -- ** Paginated
    , paginate
    -- ** Eventual consistency
    , await
    -- ** Pre-signing URLs
    , presign
    , presignURL

    -- * Environment
    , Env
    -- ** Lenses
    , envRegion
    , envLogger
    , envRetryCheck
    , envRetryPolicy
    , envManager
    , envAuth
    -- ** Creating the environment
    , newEnv
    , getEnv
    -- ** Specifying credentials
    , Credentials (..)
    , fromKeys
    , fromSession
    , getAuth
    , accessKey
    , secretKey

    -- * Logging
    , newLogger

    -- ** Streaming body helpers
    , sourceBody
    , sourceHandle
    , sourceFile
    , sourceFileIO

    -- * Types
    , module Network.AWS.Types
    , module Network.AWS.Error
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Conduit                 hiding (await)
import           Data.Monoid
import           Data.Time                    (getCurrentTime)
import           Network.AWS.Data
import           Network.AWS.Error
import           Network.AWS.Internal.Auth
import           Network.AWS.Internal.Body
import           Network.AWS.Internal.Env
import           Network.AWS.Internal.Log
import           Network.AWS.Internal.Retry
import qualified Network.AWS.Signing          as Sign
import           Network.AWS.Types
import           Network.AWS.Waiters
import           Network.HTTP.Conduit         hiding (Request, Response)
import qualified Network.HTTP.Conduit         as Client

-- | This creates a new environment without debug logging and uses 'getAuth'
-- to expand/discover the supplied 'Credentials'.
--
-- Lenses such as 'envLogger' can be used to modify the 'Env' with a debug logger.
newEnv :: (Functor m, MonadIO m)
       => Region
       -> Credentials
       -> Manager
       -> ExceptT String m Env
newEnv r c m = Env r logger check Nothing m `liftM` getAuth m c
  where
    logger _ _ = return ()
    check  _ _ = return True

-- | Create a new environment in the specified 'Region' with silent log output
-- and a new 'Manager'.
--
-- Any errors are thrown using 'error'.
--
-- /See:/ 'newEnv' for safe 'Env' instantiation.
getEnv :: Region -> Credentials -> IO Env
getEnv r c = do
    m <- newManager conduitManagerSettings
    runExceptT (newEnv r c m) >>= either error return

-- | Send a data type which is an instance of 'AWSRequest', returning either the
-- associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- This includes 'HTTPExceptions', serialisation errors, and any service
-- errors returned as part of the 'Response'.
--
-- /Note:/ Requests will be retried depending upon each service's respective
-- strategy. This can be overriden using 'envRetry'. Requests which contain
-- streaming request bodies (such as S3's 'PutObject') are never considered for retries.
send :: (MonadCatch m, MonadResource m, AWSRequest a)
     => Env
     -> a
     -> m (Response a)
send e@Env{..} (request -> rq) = fmap snd <$> retrier e rq (raw e rq)

-- | Poll the API until a predefined condition is fulfilled using the
-- supplied 'Wait' specification from the respective service.
--
-- The response will be either the first error returned that is not handled
-- by the specification, or the successful response from the await request.
--
-- /Note:/ You can find any available 'Wait' specifications under then
-- @Network.AWS.<ServiceName>.Waiters@ namespace for supported services.
await :: (MonadCatch m, MonadResource m, AWSRequest a)
      => Env
      -> Wait a
      -> a
      -> m (Response a)
await e w (request -> rq) = fmap snd <$> waiter e w rq (raw e rq)

-- | Send a data type which is an instance of 'AWSPager' and paginate over
-- the associated 'Rs' response type in the success case, or the related service's
-- 'Er' type in the error case.
--
-- /Note:/ The 'ResumableSource' will close when there are no more results or the
-- 'ResourceT' computation is unwrapped. See: 'runResourceT' for more information.
paginate :: (MonadCatch m, MonadResource m, AWSPager a)
         => Env
         -> a
         -> Source m (Response a)
paginate e = go
  where
    go x = do
        y <- lift (send e x)
        yield y
        either (const (return ()))
               (maybe (return ()) go . page x)
               y

-- | Presign an HTTP request that expires at the specified amount of time
-- in the future.
--
-- /Note:/ Requires the service's signer to be an instance of 'AWSPresigner'.
-- Not all signing process support this.
presign :: (MonadIO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
        => Env
        -> a       -- ^ Request to presign.
        -> UTCTime -- ^ Signing time.
        -> Integer -- ^ Expiry time in seconds.
        -> m Client.Request
presign Env{..} (request -> rq) t ex =
    _sgRequest `liftM` Sign.presign _envAuth _envRegion rq t ex

-- | Presign a URL that expires at the specified amount of time in the future.
--
-- /See:/ 'presign'
presignURL :: (MonadIO m, AWSRequest a, AWSPresigner (Sg (Sv a)))
           => Env
           -> a       -- ^ Request to presign.
           -> UTCTime -- ^ Signing time.
           -> Integer -- ^ Expiry time in seconds.
           -> m ByteString
presignURL e x t ex = (toBS . uri) `liftM` presign e x t ex
  where
    uri rq =
           scheme (secure rq)
        <> build  (host rq)
        <> port'  (port rq)
        <> build  (path rq)
        <> build  (queryString rq)

    scheme True = "https://"
    scheme _    = "http://"

    port' = \case
        80  -> ""
        443 -> ""
        n   -> build ':' <> build n

raw :: (MonadCatch m, MonadResource m, AWSRequest a)
    => Env
    -> Request a
    -> m (Response' a)
raw Env{..} rq = catch go err >>= response _envLogger rq
  where
    go = do
        trace _envLogger (build rq)
        t  <- liftIO getCurrentTime
        Signed m s <- Sign.sign _envAuth _envRegion rq t
        debug _envLogger (build s)
        trace _envLogger (build m)
        rs <- liftResourceT (http s _envManager)
        debug _envLogger (build rs)
        return (Right rs)

    err ex = return (Left (ex :: HttpException))
