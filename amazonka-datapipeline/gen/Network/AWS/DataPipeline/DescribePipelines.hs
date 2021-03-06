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

-- Module      : Network.AWS.DataPipeline.DescribePipelines
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

-- | Retrieves metadata about one or more pipelines. The information retrieved
-- includes the name of the pipeline, the pipeline identifier, its current
-- state, and the user account that owns the pipeline. Using account
-- credentials, you can retrieve metadata about pipelines that you or your IAM
-- users have created. If you are using an IAM user account, you can retrieve
-- metadata about only those pipelines for which you have read permissions.
--
-- To retrieve the full pipeline definition instead of metadata about the
-- pipeline, call 'GetPipelineDefinition'.
--
-- <http://docs.aws.amazon.com/datapipeline/latest/APIReference/API_DescribePipelines.html>
module Network.AWS.DataPipeline.DescribePipelines
    (
    -- * Request
      DescribePipelines
    -- ** Request constructor
    , describePipelines
    -- ** Request lenses
    , dpPipelineIds

    -- * Response
    , DescribePipelinesResponse
    -- ** Response constructor
    , describePipelinesResponse
    -- ** Response lenses
    , dprPipelineDescriptionList
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.DataPipeline.Types
import qualified GHC.Exts

newtype DescribePipelines = DescribePipelines
    { _dpPipelineIds :: List "pipelineIds" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribePipelines where
    type Item DescribePipelines = Text

    fromList = DescribePipelines . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dpPipelineIds

-- | 'DescribePipelines' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpPipelineIds' @::@ ['Text']
--
describePipelines :: DescribePipelines
describePipelines = DescribePipelines
    { _dpPipelineIds = mempty
    }

-- | The IDs of the pipelines to describe. You can pass as many as 25 identifiers
-- in a single call. To obtain pipeline IDs, call 'ListPipelines'.
dpPipelineIds :: Lens' DescribePipelines [Text]
dpPipelineIds = lens _dpPipelineIds (\s a -> s { _dpPipelineIds = a }) . _List

newtype DescribePipelinesResponse = DescribePipelinesResponse
    { _dprPipelineDescriptionList :: List "pipelineDescriptionList" PipelineDescription
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DescribePipelinesResponse where
    type Item DescribePipelinesResponse = PipelineDescription

    fromList = DescribePipelinesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dprPipelineDescriptionList

-- | 'DescribePipelinesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dprPipelineDescriptionList' @::@ ['PipelineDescription']
--
describePipelinesResponse :: DescribePipelinesResponse
describePipelinesResponse = DescribePipelinesResponse
    { _dprPipelineDescriptionList = mempty
    }

-- | An array of descriptions for the specified pipelines.
dprPipelineDescriptionList :: Lens' DescribePipelinesResponse [PipelineDescription]
dprPipelineDescriptionList =
    lens _dprPipelineDescriptionList
        (\s a -> s { _dprPipelineDescriptionList = a })
            . _List

instance ToPath DescribePipelines where
    toPath = const "/"

instance ToQuery DescribePipelines where
    toQuery = const mempty

instance ToHeaders DescribePipelines

instance ToJSON DescribePipelines where
    toJSON DescribePipelines{..} = object
        [ "pipelineIds" .= _dpPipelineIds
        ]

instance AWSRequest DescribePipelines where
    type Sv DescribePipelines = DataPipeline
    type Rs DescribePipelines = DescribePipelinesResponse

    request  = post "DescribePipelines"
    response = jsonResponse

instance FromJSON DescribePipelinesResponse where
    parseJSON = withObject "DescribePipelinesResponse" $ \o -> DescribePipelinesResponse
        <$> o .:? "pipelineDescriptionList" .!= mempty
