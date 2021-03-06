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

-- Module      : Network.AWS.RDS.AddTagsToResource
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

-- | Adds metadata tags to an Amazon RDS resource. These tags can also be used
-- with cost allocation reporting to track cost associated with Amazon RDS
-- resources, or used in Condition statement in IAM policy for Amazon RDS.
--
-- For an overview on tagging Amazon RDS resources, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDSResources>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AddTagsToResource.html>
module Network.AWS.RDS.AddTagsToResource
    (
    -- * Request
      AddTagsToResource
    -- ** Request constructor
    , addTagsToResource
    -- ** Request lenses
    , attrResourceName
    , attrTags

    -- * Response
    , AddTagsToResourceResponse
    -- ** Response constructor
    , addTagsToResourceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data AddTagsToResource = AddTagsToResource
    { _attrResourceName :: Text
    , _attrTags         :: List "member" Tag
    } deriving (Eq, Read, Show)

-- | 'AddTagsToResource' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'attrResourceName' @::@ 'Text'
--
-- * 'attrTags' @::@ ['Tag']
--
addTagsToResource :: Text -- ^ 'attrResourceName'
                  -> AddTagsToResource
addTagsToResource p1 = AddTagsToResource
    { _attrResourceName = p1
    , _attrTags         = mempty
    }

-- | The Amazon RDS resource the tags will be added to. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN  Constructingan RDS Amazon Resource Name (ARN)>.
attrResourceName :: Lens' AddTagsToResource Text
attrResourceName = lens _attrResourceName (\s a -> s { _attrResourceName = a })

-- | The tags to be assigned to the Amazon RDS resource.
attrTags :: Lens' AddTagsToResource [Tag]
attrTags = lens _attrTags (\s a -> s { _attrTags = a }) . _List

data AddTagsToResourceResponse = AddTagsToResourceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AddTagsToResourceResponse' constructor.
addTagsToResourceResponse :: AddTagsToResourceResponse
addTagsToResourceResponse = AddTagsToResourceResponse

instance ToPath AddTagsToResource where
    toPath = const "/"

instance ToQuery AddTagsToResource where
    toQuery AddTagsToResource{..} = mconcat
        [ "ResourceName" =? _attrResourceName
        , "Tags"         =? _attrTags
        ]

instance ToHeaders AddTagsToResource

instance AWSRequest AddTagsToResource where
    type Sv AddTagsToResource = RDS
    type Rs AddTagsToResource = AddTagsToResourceResponse

    request  = post "AddTagsToResource"
    response = nullResponse AddTagsToResourceResponse
