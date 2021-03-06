{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.DeleteBucketTagging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the tags from the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteBucketTagging.html>
module Network.AWS.S3.DeleteBucketTagging
    (
    -- * Request
      DeleteBucketTagging
    -- ** Request constructor
    , deleteBucketTagging
    -- ** Request lenses
    , dbtBucket

    -- * Response
    , DeleteBucketTaggingResponse
    -- ** Response constructor
    , deleteBucketTaggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype DeleteBucketTagging = DeleteBucketTagging
    { _dbtBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'DeleteBucketTagging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dbtBucket' @::@ 'Text'
--
deleteBucketTagging :: Text -- ^ 'dbtBucket'
                    -> DeleteBucketTagging
deleteBucketTagging p1 = DeleteBucketTagging
    { _dbtBucket = p1
    }

dbtBucket :: Lens' DeleteBucketTagging Text
dbtBucket = lens _dbtBucket (\s a -> s { _dbtBucket = a })

data DeleteBucketTaggingResponse = DeleteBucketTaggingResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteBucketTaggingResponse' constructor.
deleteBucketTaggingResponse :: DeleteBucketTaggingResponse
deleteBucketTaggingResponse = DeleteBucketTaggingResponse

instance ToPath DeleteBucketTagging where
    toPath DeleteBucketTagging{..} = mconcat
        [ "/"
        , toText _dbtBucket
        ]

instance ToQuery DeleteBucketTagging where
    toQuery = const "tagging"

instance ToHeaders DeleteBucketTagging

instance ToXMLRoot DeleteBucketTagging where
    toXMLRoot = const (namespaced ns "DeleteBucketTagging" [])

instance ToXML DeleteBucketTagging

instance AWSRequest DeleteBucketTagging where
    type Sv DeleteBucketTagging = S3
    type Rs DeleteBucketTagging = DeleteBucketTaggingResponse

    request  = delete
    response = nullResponse DeleteBucketTaggingResponse
