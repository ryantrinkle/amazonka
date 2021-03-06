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

-- Module      : Network.AWS.ElastiCache.DescribeReservedCacheNodes
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

-- | The /DescribeReservedCacheNodes/ action returns information about reserved
-- cache nodes for this account, or about a specified reserved cache node.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeReservedCacheNodes.html>
module Network.AWS.ElastiCache.DescribeReservedCacheNodes
    (
    -- * Request
      DescribeReservedCacheNodes
    -- ** Request constructor
    , describeReservedCacheNodes
    -- ** Request lenses
    , drcnCacheNodeType
    , drcnDuration
    , drcnMarker
    , drcnMaxRecords
    , drcnOfferingType
    , drcnProductDescription
    , drcnReservedCacheNodeId
    , drcnReservedCacheNodesOfferingId

    -- * Response
    , DescribeReservedCacheNodesResponse
    -- ** Response constructor
    , describeReservedCacheNodesResponse
    -- ** Response lenses
    , drcnrMarker
    , drcnrReservedCacheNodes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnCacheNodeType                :: Maybe Text
    , _drcnDuration                     :: Maybe Text
    , _drcnMarker                       :: Maybe Text
    , _drcnMaxRecords                   :: Maybe Int
    , _drcnOfferingType                 :: Maybe Text
    , _drcnProductDescription           :: Maybe Text
    , _drcnReservedCacheNodeId          :: Maybe Text
    , _drcnReservedCacheNodesOfferingId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeReservedCacheNodes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'drcnDuration' @::@ 'Maybe' 'Text'
--
-- * 'drcnMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'drcnOfferingType' @::@ 'Maybe' 'Text'
--
-- * 'drcnProductDescription' @::@ 'Maybe' 'Text'
--
-- * 'drcnReservedCacheNodeId' @::@ 'Maybe' 'Text'
--
-- * 'drcnReservedCacheNodesOfferingId' @::@ 'Maybe' 'Text'
--
describeReservedCacheNodes :: DescribeReservedCacheNodes
describeReservedCacheNodes = DescribeReservedCacheNodes
    { _drcnReservedCacheNodeId          = Nothing
    , _drcnReservedCacheNodesOfferingId = Nothing
    , _drcnCacheNodeType                = Nothing
    , _drcnDuration                     = Nothing
    , _drcnProductDescription           = Nothing
    , _drcnOfferingType                 = Nothing
    , _drcnMaxRecords                   = Nothing
    , _drcnMarker                       = Nothing
    }

-- | The cache node type filter value. Use this parameter to show only those
-- reservations matching the specified cache node type.
--
-- Valid node types are as follows:
--
-- General purpose:  Current generation: 'cache.t2.micro', 'cache.t2.small', 'cache.t2.medium', 'cache.m3.medium', 'cache.m3.large', 'cache.m3.xlarge', 'cache.m3.2xlarge' Previous
-- generation: 'cache.t1.micro', 'cache.m1.small', 'cache.m1.medium', 'cache.m1.large', 'cache.m1.xlarge'  Compute optimized: 'cache.c1.xlarge' Memory optimized  Current generation: 'cache.r3.large', 'cache.r3.xlarge', 'cache.r3.2xlarge', 'cache.r3.4xlarge', 'cache.r3.8xlarge' Previous generation:
-- 'cache.m2.xlarge', 'cache.m2.2xlarge', 'cache.m2.4xlarge'   Notes:
--
-- All t2 instances are created in an Amazon Virtual Private Cloud (VPC). Redis backup/restore is not supported for t2 instances.
-- Redis Append-only files (AOF) functionality is not supported for t1 or t2
-- instances.  For a complete listing of cache node types and specifications,
-- see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache NodeType-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parametersfor Redis>.
drcnCacheNodeType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnCacheNodeType =
    lens _drcnCacheNodeType (\s a -> s { _drcnCacheNodeType = a })

-- | The duration filter value, specified in years or seconds. Use this parameter
-- to show only reservations for this duration.
--
-- Valid Values: '1 | 3 | 31536000 | 94608000'
drcnDuration :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnDuration = lens _drcnDuration (\s a -> s { _drcnDuration = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified, the
-- response includes only records beyond the marker, up to the value specified
-- by /MaxRecords/.
drcnMarker :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnMarker = lens _drcnMarker (\s a -> s { _drcnMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a marker is included in the
-- response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
drcnMaxRecords :: Lens' DescribeReservedCacheNodes (Maybe Int)
drcnMaxRecords = lens _drcnMaxRecords (\s a -> s { _drcnMaxRecords = a })

-- | The offering type filter value. Use this parameter to show only the available
-- offerings matching the specified offering type.
--
-- Valid values: '"Light Utilization"|"Medium Utilization"|"Heavy Utilization"'
drcnOfferingType :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnOfferingType = lens _drcnOfferingType (\s a -> s { _drcnOfferingType = a })

-- | The product description filter value. Use this parameter to show only those
-- reservations matching the specified product description.
drcnProductDescription :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnProductDescription =
    lens _drcnProductDescription (\s a -> s { _drcnProductDescription = a })

-- | The reserved cache node identifier filter value. Use this parameter to show
-- only the reservation that matches the specified reservation ID.
drcnReservedCacheNodeId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodeId =
    lens _drcnReservedCacheNodeId (\s a -> s { _drcnReservedCacheNodeId = a })

-- | The offering identifier filter value. Use this parameter to show only
-- purchased reservations matching the specified offering identifier.
drcnReservedCacheNodesOfferingId :: Lens' DescribeReservedCacheNodes (Maybe Text)
drcnReservedCacheNodesOfferingId =
    lens _drcnReservedCacheNodesOfferingId
        (\s a -> s { _drcnReservedCacheNodesOfferingId = a })

data DescribeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _drcnrMarker             :: Maybe Text
    , _drcnrReservedCacheNodes :: List "member" ReservedCacheNode
    } deriving (Eq, Read, Show)

-- | 'DescribeReservedCacheNodesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drcnrMarker' @::@ 'Maybe' 'Text'
--
-- * 'drcnrReservedCacheNodes' @::@ ['ReservedCacheNode']
--
describeReservedCacheNodesResponse :: DescribeReservedCacheNodesResponse
describeReservedCacheNodesResponse = DescribeReservedCacheNodesResponse
    { _drcnrMarker             = Nothing
    , _drcnrReservedCacheNodes = mempty
    }

-- | Provides an identifier to allow retrieval of paginated results.
drcnrMarker :: Lens' DescribeReservedCacheNodesResponse (Maybe Text)
drcnrMarker = lens _drcnrMarker (\s a -> s { _drcnrMarker = a })

-- | A list of reserved cache nodes. Each element in the list contains detailed
-- information about one node.
drcnrReservedCacheNodes :: Lens' DescribeReservedCacheNodesResponse [ReservedCacheNode]
drcnrReservedCacheNodes =
    lens _drcnrReservedCacheNodes (\s a -> s { _drcnrReservedCacheNodes = a })
        . _List

instance ToPath DescribeReservedCacheNodes where
    toPath = const "/"

instance ToQuery DescribeReservedCacheNodes where
    toQuery DescribeReservedCacheNodes{..} = mconcat
        [ "CacheNodeType"                =? _drcnCacheNodeType
        , "Duration"                     =? _drcnDuration
        , "Marker"                       =? _drcnMarker
        , "MaxRecords"                   =? _drcnMaxRecords
        , "OfferingType"                 =? _drcnOfferingType
        , "ProductDescription"           =? _drcnProductDescription
        , "ReservedCacheNodeId"          =? _drcnReservedCacheNodeId
        , "ReservedCacheNodesOfferingId" =? _drcnReservedCacheNodesOfferingId
        ]

instance ToHeaders DescribeReservedCacheNodes

instance AWSRequest DescribeReservedCacheNodes where
    type Sv DescribeReservedCacheNodes = ElastiCache
    type Rs DescribeReservedCacheNodes = DescribeReservedCacheNodesResponse

    request  = post "DescribeReservedCacheNodes"
    response = xmlResponse

instance FromXML DescribeReservedCacheNodesResponse where
    parseXML = withElement "DescribeReservedCacheNodesResult" $ \x -> DescribeReservedCacheNodesResponse
        <$> x .@? "Marker"
        <*> x .@? "ReservedCacheNodes" .!@ mempty

instance AWSPager DescribeReservedCacheNodes where
    page rq rs
        | stop (rs ^. drcnrMarker) = Nothing
        | otherwise = (\x -> rq & drcnMarker ?~ x)
            <$> (rs ^. drcnrMarker)
