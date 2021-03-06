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

-- Module      : Network.AWS.Route53Domains.GetOperationDetail
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

-- | This operation returns the current status of an operation that is not
-- completed.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-GetOperationDetail.html>
module Network.AWS.Route53Domains.GetOperationDetail
    (
    -- * Request
      GetOperationDetail
    -- ** Request constructor
    , getOperationDetail
    -- ** Request lenses
    , godOperationId

    -- * Response
    , GetOperationDetailResponse
    -- ** Response constructor
    , getOperationDetailResponse
    -- ** Response lenses
    , godrDomainName
    , godrMessage
    , godrOperationId
    , godrStatus
    , godrSubmittedDate
    , godrType
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

newtype GetOperationDetail = GetOperationDetail
    { _godOperationId :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetOperationDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godOperationId' @::@ 'Text'
--
getOperationDetail :: Text -- ^ 'godOperationId'
                   -> GetOperationDetail
getOperationDetail p1 = GetOperationDetail
    { _godOperationId = p1
    }

-- | The identifier for the operation for which you want to get the status. Amazon
-- Route 53 returned the identifier in the response to the original request.
--
-- Type: String
--
-- Default: None
--
-- Required: Yes
godOperationId :: Lens' GetOperationDetail Text
godOperationId = lens _godOperationId (\s a -> s { _godOperationId = a })

data GetOperationDetailResponse = GetOperationDetailResponse
    { _godrDomainName    :: Maybe Text
    , _godrMessage       :: Maybe Text
    , _godrOperationId   :: Maybe Text
    , _godrStatus        :: Maybe OperationStatus
    , _godrSubmittedDate :: Maybe POSIX
    , _godrType          :: Maybe OperationType
    } deriving (Eq, Read, Show)

-- | 'GetOperationDetailResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'godrDomainName' @::@ 'Maybe' 'Text'
--
-- * 'godrMessage' @::@ 'Maybe' 'Text'
--
-- * 'godrOperationId' @::@ 'Maybe' 'Text'
--
-- * 'godrStatus' @::@ 'Maybe' 'OperationStatus'
--
-- * 'godrSubmittedDate' @::@ 'Maybe' 'UTCTime'
--
-- * 'godrType' @::@ 'Maybe' 'OperationType'
--
getOperationDetailResponse :: GetOperationDetailResponse
getOperationDetailResponse = GetOperationDetailResponse
    { _godrOperationId   = Nothing
    , _godrStatus        = Nothing
    , _godrMessage       = Nothing
    , _godrDomainName    = Nothing
    , _godrType          = Nothing
    , _godrSubmittedDate = Nothing
    }

-- | The name of a domain.
--
-- Type: String
godrDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrDomainName = lens _godrDomainName (\s a -> s { _godrDomainName = a })

-- | Detailed information on the status including possible errors.
--
-- Type: String
godrMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrMessage = lens _godrMessage (\s a -> s { _godrMessage = a })

-- | The identifier for the operation.
--
-- Type: String
godrOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrOperationId = lens _godrOperationId (\s a -> s { _godrOperationId = a })

-- | The current status of the requested operation in the system.
--
-- Type: String
godrStatus :: Lens' GetOperationDetailResponse (Maybe OperationStatus)
godrStatus = lens _godrStatus (\s a -> s { _godrStatus = a })

-- | The date when the request was submitted.
godrSubmittedDate :: Lens' GetOperationDetailResponse (Maybe UTCTime)
godrSubmittedDate =
    lens _godrSubmittedDate (\s a -> s { _godrSubmittedDate = a })
        . mapping _Time

-- | The type of operation that was requested.
--
-- Type: String
godrType :: Lens' GetOperationDetailResponse (Maybe OperationType)
godrType = lens _godrType (\s a -> s { _godrType = a })

instance ToPath GetOperationDetail where
    toPath = const "/"

instance ToQuery GetOperationDetail where
    toQuery = const mempty

instance ToHeaders GetOperationDetail

instance ToJSON GetOperationDetail where
    toJSON GetOperationDetail{..} = object
        [ "OperationId" .= _godOperationId
        ]

instance AWSRequest GetOperationDetail where
    type Sv GetOperationDetail = Route53Domains
    type Rs GetOperationDetail = GetOperationDetailResponse

    request  = post "GetOperationDetail"
    response = jsonResponse

instance FromJSON GetOperationDetailResponse where
    parseJSON = withObject "GetOperationDetailResponse" $ \o -> GetOperationDetailResponse
        <$> o .:? "DomainName"
        <*> o .:? "Message"
        <*> o .:? "OperationId"
        <*> o .:? "Status"
        <*> o .:? "SubmittedDate"
        <*> o .:? "Type"
