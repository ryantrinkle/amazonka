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

-- Module      : Network.AWS.S3.GetBucketNotification
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

-- | Deprecated, see the GetBucketNotificationConfiguration operation.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketNotification.html>
module Network.AWS.S3.GetBucketNotification
    (
    -- * Request
      GetBucketNotification
    -- ** Request constructor
    , getBucketNotification
    -- ** Request lenses
    , gbnBucket

    -- * Response
    , GetBucketNotificationResponse
    -- ** Response constructor
    , getBucketNotificationResponse
    -- ** Response lenses
    , gbnrCloudFunctionConfiguration
    , gbnrQueueConfiguration
    , gbnrTopicConfiguration
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.S3
import Network.AWS.S3.Types
import qualified GHC.Exts

newtype GetBucketNotification = GetBucketNotification
    { _gbnBucket :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetBucketNotification' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnBucket' @::@ 'Text'
--
getBucketNotification :: Text -- ^ 'gbnBucket'
                      -> GetBucketNotification
getBucketNotification p1 = GetBucketNotification
    { _gbnBucket = p1
    }

-- | Name of the buket to get the notification configuration for.
gbnBucket :: Lens' GetBucketNotification Text
gbnBucket = lens _gbnBucket (\s a -> s { _gbnBucket = a })

data GetBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnrCloudFunctionConfiguration :: Maybe CloudFunctionConfiguration
    , _gbnrQueueConfiguration         :: Maybe QueueConfigurationDeprecated
    , _gbnrTopicConfiguration         :: Maybe TopicConfigurationDeprecated
    } deriving (Eq, Read, Show)

-- | 'GetBucketNotificationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbnrCloudFunctionConfiguration' @::@ 'Maybe' 'CloudFunctionConfiguration'
--
-- * 'gbnrQueueConfiguration' @::@ 'Maybe' 'QueueConfigurationDeprecated'
--
-- * 'gbnrTopicConfiguration' @::@ 'Maybe' 'TopicConfigurationDeprecated'
--
getBucketNotificationResponse :: GetBucketNotificationResponse
getBucketNotificationResponse = GetBucketNotificationResponse
    { _gbnrTopicConfiguration         = Nothing
    , _gbnrQueueConfiguration         = Nothing
    , _gbnrCloudFunctionConfiguration = Nothing
    }

gbnrCloudFunctionConfiguration :: Lens' GetBucketNotificationResponse (Maybe CloudFunctionConfiguration)
gbnrCloudFunctionConfiguration =
    lens _gbnrCloudFunctionConfiguration
        (\s a -> s { _gbnrCloudFunctionConfiguration = a })

gbnrQueueConfiguration :: Lens' GetBucketNotificationResponse (Maybe QueueConfigurationDeprecated)
gbnrQueueConfiguration =
    lens _gbnrQueueConfiguration (\s a -> s { _gbnrQueueConfiguration = a })

gbnrTopicConfiguration :: Lens' GetBucketNotificationResponse (Maybe TopicConfigurationDeprecated)
gbnrTopicConfiguration =
    lens _gbnrTopicConfiguration (\s a -> s { _gbnrTopicConfiguration = a })

instance ToPath GetBucketNotification where
    toPath GetBucketNotification{..} = mconcat
        [ "/"
        , toText _gbnBucket
        ]

instance ToQuery GetBucketNotification where
    toQuery = const "notification"

instance ToHeaders GetBucketNotification

instance ToXMLRoot GetBucketNotification where
    toXMLRoot = const (namespaced ns "GetBucketNotification" [])

instance ToXML GetBucketNotification

instance AWSRequest GetBucketNotification where
    type Sv GetBucketNotification = S3
    type Rs GetBucketNotification = GetBucketNotificationResponse

    request  = get
    response = xmlResponse

instance FromXML GetBucketNotificationResponse where
    parseXML x = GetBucketNotificationResponse
        <$> x .@? "CloudFunctionConfiguration"
        <*> x .@? "QueueConfiguration"
        <*> x .@? "TopicConfiguration"
