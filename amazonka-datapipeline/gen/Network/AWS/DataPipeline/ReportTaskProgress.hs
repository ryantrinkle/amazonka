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

-- Module      : Network.AWS.DataPipeline.ReportTaskProgress
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

-- | Task runners call 'ReportTaskProgress' when assigned a task to acknowledge that
-- it has the task. If the web service does not receive this acknowledgement
-- within 2 minutes, it assigns the task in a subsequent 'PollForTask' call. After
-- this initial acknowledgement, the task runner only needs to report progress
-- every 15 minutes to maintain its ownership of the task. You can change this
-- reporting time from 15 minutes by specifying a 'reportProgressTimeout' field in
-- your pipeline.
--
-- If a task runner does not report its status after 5 minutes, AWS Data
-- Pipeline assumes that the task runner is unable to process the task and
-- reassigns the task in a subsequent response to 'PollForTask'. Task runners
-- should call 'ReportTaskProgress' every 60 seconds.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_ReportTaskProgress.html>
module Network.AWS.DataPipeline.ReportTaskProgress
    (
    -- * Request
      ReportTaskProgress
    -- ** Request constructor
    , reportTaskProgress
    -- ** Request lenses
    , rtpFields
    , rtpTaskId

    -- * Response
    , ReportTaskProgressResponse
    -- ** Response constructor
    , reportTaskProgressResponse
    -- ** Response lenses
    , rtprCanceled
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

data ReportTaskProgress = ReportTaskProgress
    { _rtpFields :: List "fields" Field
    , _rtpTaskId :: Text
    } deriving (Eq, Read, Show)

-- | 'ReportTaskProgress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtpFields' @::@ ['Field']
--
-- * 'rtpTaskId' @::@ 'Text'
--
reportTaskProgress :: Text -- ^ 'rtpTaskId'
                   -> ReportTaskProgress
reportTaskProgress p1 = ReportTaskProgress
    { _rtpTaskId = p1
    , _rtpFields = mempty
    }

-- | Key-value pairs that define the properties of the ReportTaskProgressInput
-- object.
rtpFields :: Lens' ReportTaskProgress [Field]
rtpFields = lens _rtpFields (\s a -> s { _rtpFields = a }) . _List

-- | The ID of the task assigned to the task runner. This value is provided in the
-- response for 'PollForTask'.
rtpTaskId :: Lens' ReportTaskProgress Text
rtpTaskId = lens _rtpTaskId (\s a -> s { _rtpTaskId = a })

newtype ReportTaskProgressResponse = ReportTaskProgressResponse
    { _rtprCanceled :: Bool
    } deriving (Eq, Ord, Read, Show, Enum)

-- | 'ReportTaskProgressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtprCanceled' @::@ 'Bool'
--
reportTaskProgressResponse :: Bool -- ^ 'rtprCanceled'
                           -> ReportTaskProgressResponse
reportTaskProgressResponse p1 = ReportTaskProgressResponse
    { _rtprCanceled = p1
    }

-- | If true, the calling task runner should cancel processing of the task. The
-- task runner does not need to call 'SetTaskStatus' for canceled tasks.
rtprCanceled :: Lens' ReportTaskProgressResponse Bool
rtprCanceled = lens _rtprCanceled (\s a -> s { _rtprCanceled = a })

instance ToPath ReportTaskProgress where
    toPath = const "/"

instance ToQuery ReportTaskProgress where
    toQuery = const mempty

instance ToHeaders ReportTaskProgress

instance ToJSON ReportTaskProgress where
    toJSON ReportTaskProgress{..} = object
        [ "taskId" .= _rtpTaskId
        , "fields" .= _rtpFields
        ]

instance AWSRequest ReportTaskProgress where
    type Sv ReportTaskProgress = DataPipeline
    type Rs ReportTaskProgress = ReportTaskProgressResponse

    request  = post "ReportTaskProgress"
    response = jsonResponse

instance FromJSON ReportTaskProgressResponse where
    parseJSON = withObject "ReportTaskProgressResponse" $ \o -> ReportTaskProgressResponse
        <$> o .:  "canceled"
