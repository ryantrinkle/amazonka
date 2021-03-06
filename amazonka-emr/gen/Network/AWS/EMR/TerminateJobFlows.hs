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

-- Module      : Network.AWS.EMR.TerminateJobFlows
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

-- | TerminateJobFlows shuts a list of job flows down. When a job flow is shut
-- down, any step not yet completed is canceled and the EC2 instances on which
-- the job flow is running are stopped. Any log files not already saved are
-- uploaded to Amazon S3 if a LogUri was specified when the job flow was
-- created.
--
-- The maximum number of JobFlows allowed is 10. The call to TerminateJobFlows
-- is asynchronous. Depending on the configuration of the job flow, it may take
-- up to 5-20 minutes for the job flow to completely terminate and release
-- allocated resources, such as Amazon EC2 instances.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_TerminateJobFlows.html>
module Network.AWS.EMR.TerminateJobFlows
    (
    -- * Request
      TerminateJobFlows
    -- ** Request constructor
    , terminateJobFlows
    -- ** Request lenses
    , tjfJobFlowIds

    -- * Response
    , TerminateJobFlowsResponse
    -- ** Response constructor
    , terminateJobFlowsResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

newtype TerminateJobFlows = TerminateJobFlows
    { _tjfJobFlowIds :: List "JobFlowIds" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList TerminateJobFlows where
    type Item TerminateJobFlows = Text

    fromList = TerminateJobFlows . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _tjfJobFlowIds

-- | 'TerminateJobFlows' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tjfJobFlowIds' @::@ ['Text']
--
terminateJobFlows :: TerminateJobFlows
terminateJobFlows = TerminateJobFlows
    { _tjfJobFlowIds = mempty
    }

-- | A list of job flows to be shutdown.
tjfJobFlowIds :: Lens' TerminateJobFlows [Text]
tjfJobFlowIds = lens _tjfJobFlowIds (\s a -> s { _tjfJobFlowIds = a }) . _List

data TerminateJobFlowsResponse = TerminateJobFlowsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'TerminateJobFlowsResponse' constructor.
terminateJobFlowsResponse :: TerminateJobFlowsResponse
terminateJobFlowsResponse = TerminateJobFlowsResponse

instance ToPath TerminateJobFlows where
    toPath = const "/"

instance ToQuery TerminateJobFlows where
    toQuery = const mempty

instance ToHeaders TerminateJobFlows

instance ToJSON TerminateJobFlows where
    toJSON TerminateJobFlows{..} = object
        [ "JobFlowIds" .= _tjfJobFlowIds
        ]

instance AWSRequest TerminateJobFlows where
    type Sv TerminateJobFlows = EMR
    type Rs TerminateJobFlows = TerminateJobFlowsResponse

    request  = post "TerminateJobFlows"
    response = nullResponse TerminateJobFlowsResponse
