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

-- Module      : Network.AWS.RDS.ModifyOptionGroup
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

-- | Modifies an existing option group.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyOptionGroup.html>
module Network.AWS.RDS.ModifyOptionGroup
    (
    -- * Request
      ModifyOptionGroup
    -- ** Request constructor
    , modifyOptionGroup
    -- ** Request lenses
    , mogApplyImmediately
    , mogOptionGroupName
    , mogOptionsToInclude
    , mogOptionsToRemove

    -- * Response
    , ModifyOptionGroupResponse
    -- ** Response constructor
    , modifyOptionGroupResponse
    -- ** Response lenses
    , mogrOptionGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data ModifyOptionGroup = ModifyOptionGroup
    { _mogApplyImmediately :: Maybe Bool
    , _mogOptionGroupName  :: Text
    , _mogOptionsToInclude :: List "member" OptionConfiguration
    , _mogOptionsToRemove  :: List "member" Text
    } deriving (Eq, Read, Show)

-- | 'ModifyOptionGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogApplyImmediately' @::@ 'Maybe' 'Bool'
--
-- * 'mogOptionGroupName' @::@ 'Text'
--
-- * 'mogOptionsToInclude' @::@ ['OptionConfiguration']
--
-- * 'mogOptionsToRemove' @::@ ['Text']
--
modifyOptionGroup :: Text -- ^ 'mogOptionGroupName'
                  -> ModifyOptionGroup
modifyOptionGroup p1 = ModifyOptionGroup
    { _mogOptionGroupName  = p1
    , _mogOptionsToInclude = mempty
    , _mogOptionsToRemove  = mempty
    , _mogApplyImmediately = Nothing
    }

-- | Indicates whether the changes should be applied immediately, or during the
-- next maintenance window for each instance associated with the option group.
mogApplyImmediately :: Lens' ModifyOptionGroup (Maybe Bool)
mogApplyImmediately =
    lens _mogApplyImmediately (\s a -> s { _mogApplyImmediately = a })

-- | The name of the option group to be modified.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security TDE,
-- cannot be removed from an option group, and that option group cannot be
-- removed from a DB instance once it is associated with a DB instance
mogOptionGroupName :: Lens' ModifyOptionGroup Text
mogOptionGroupName =
    lens _mogOptionGroupName (\s a -> s { _mogOptionGroupName = a })

-- | Options in this list are added to the option group or, if already present,
-- the specified configuration is used to update the existing configuration.
mogOptionsToInclude :: Lens' ModifyOptionGroup [OptionConfiguration]
mogOptionsToInclude =
    lens _mogOptionsToInclude (\s a -> s { _mogOptionsToInclude = a })
        . _List

-- | Options in this list are removed from the option group.
mogOptionsToRemove :: Lens' ModifyOptionGroup [Text]
mogOptionsToRemove =
    lens _mogOptionsToRemove (\s a -> s { _mogOptionsToRemove = a })
        . _List

newtype ModifyOptionGroupResponse = ModifyOptionGroupResponse
    { _mogrOptionGroup :: Maybe OptionGroup
    } deriving (Eq, Read, Show)

-- | 'ModifyOptionGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mogrOptionGroup' @::@ 'Maybe' 'OptionGroup'
--
modifyOptionGroupResponse :: ModifyOptionGroupResponse
modifyOptionGroupResponse = ModifyOptionGroupResponse
    { _mogrOptionGroup = Nothing
    }

mogrOptionGroup :: Lens' ModifyOptionGroupResponse (Maybe OptionGroup)
mogrOptionGroup = lens _mogrOptionGroup (\s a -> s { _mogrOptionGroup = a })

instance ToPath ModifyOptionGroup where
    toPath = const "/"

instance ToQuery ModifyOptionGroup where
    toQuery ModifyOptionGroup{..} = mconcat
        [ "ApplyImmediately" =? _mogApplyImmediately
        , "OptionGroupName"  =? _mogOptionGroupName
        , "OptionsToInclude" =? _mogOptionsToInclude
        , "OptionsToRemove"  =? _mogOptionsToRemove
        ]

instance ToHeaders ModifyOptionGroup

instance AWSRequest ModifyOptionGroup where
    type Sv ModifyOptionGroup = RDS
    type Rs ModifyOptionGroup = ModifyOptionGroupResponse

    request  = post "ModifyOptionGroup"
    response = xmlResponse

instance FromXML ModifyOptionGroupResponse where
    parseXML = withElement "ModifyOptionGroupResult" $ \x -> ModifyOptionGroupResponse
        <$> x .@? "OptionGroup"
