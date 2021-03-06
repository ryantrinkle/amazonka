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

-- Module      : Network.AWS.AutoScaling.DeleteAutoScalingGroup
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

-- | Deletes the specified Auto Scaling group.
--
-- The group must have no instances and no scaling activities in progress.
--
-- To remove all instances before calling 'DeleteAutoScalingGroup', you can call 'UpdateAutoScalingGroup' to set the minimum and maximum size of the AutoScalingGroup to zero.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DeleteAutoScalingGroup.html>
module Network.AWS.AutoScaling.DeleteAutoScalingGroup
    (
    -- * Request
      DeleteAutoScalingGroup
    -- ** Request constructor
    , deleteAutoScalingGroup
    -- ** Request lenses
    , dasgAutoScalingGroupName
    , dasgForceDelete

    -- * Response
    , DeleteAutoScalingGroupResponse
    -- ** Response constructor
    , deleteAutoScalingGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DeleteAutoScalingGroup = DeleteAutoScalingGroup
    { _dasgAutoScalingGroupName :: Text
    , _dasgForceDelete          :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteAutoScalingGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgAutoScalingGroupName' @::@ 'Text'
--
-- * 'dasgForceDelete' @::@ 'Maybe' 'Bool'
--
deleteAutoScalingGroup :: Text -- ^ 'dasgAutoScalingGroupName'
                       -> DeleteAutoScalingGroup
deleteAutoScalingGroup p1 = DeleteAutoScalingGroup
    { _dasgAutoScalingGroupName = p1
    , _dasgForceDelete          = Nothing
    }

-- | The name of the group to delete.
dasgAutoScalingGroupName :: Lens' DeleteAutoScalingGroup Text
dasgAutoScalingGroupName =
    lens _dasgAutoScalingGroupName
        (\s a -> s { _dasgAutoScalingGroupName = a })

-- | Specifies that the group will be deleted along with all instances associated
-- with the group, without waiting for all instances to be terminated. This
-- parameter also deletes any lifecycle actions associated with the group.
dasgForceDelete :: Lens' DeleteAutoScalingGroup (Maybe Bool)
dasgForceDelete = lens _dasgForceDelete (\s a -> s { _dasgForceDelete = a })

data DeleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteAutoScalingGroupResponse' constructor.
deleteAutoScalingGroupResponse :: DeleteAutoScalingGroupResponse
deleteAutoScalingGroupResponse = DeleteAutoScalingGroupResponse

instance ToPath DeleteAutoScalingGroup where
    toPath = const "/"

instance ToQuery DeleteAutoScalingGroup where
    toQuery DeleteAutoScalingGroup{..} = mconcat
        [ "AutoScalingGroupName" =? _dasgAutoScalingGroupName
        , "ForceDelete"          =? _dasgForceDelete
        ]

instance ToHeaders DeleteAutoScalingGroup

instance AWSRequest DeleteAutoScalingGroup where
    type Sv DeleteAutoScalingGroup = AutoScaling
    type Rs DeleteAutoScalingGroup = DeleteAutoScalingGroupResponse

    request  = post "DeleteAutoScalingGroup"
    response = nullResponse DeleteAutoScalingGroupResponse
