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

-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
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

-- | This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations may not
-- immediately represent all issued operations.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DeleteTagsForDomain.html>
module Network.AWS.Route53Domains.DeleteTagsForDomain
    (
    -- * Request
      DeleteTagsForDomain
    -- ** Request constructor
    , deleteTagsForDomain
    -- ** Request lenses
    , dtfdDomainName
    , dtfdTagsToDelete

    -- * Response
    , DeleteTagsForDomainResponse
    -- ** Response constructor
    , deleteTagsForDomainResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data DeleteTagsForDomain = DeleteTagsForDomain
    { _dtfdDomainName   :: Text
    , _dtfdTagsToDelete :: List "TagsToDelete" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteTagsForDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtfdDomainName' @::@ 'Text'
--
-- * 'dtfdTagsToDelete' @::@ ['Text']
--
deleteTagsForDomain :: Text -- ^ 'dtfdDomainName'
                    -> DeleteTagsForDomain
deleteTagsForDomain p1 = DeleteTagsForDomain
    { _dtfdDomainName   = p1
    , _dtfdTagsToDelete = mempty
    }

-- | The domain for which you want to delete one or more tags.
--
-- The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z, the
-- numbers 0 through 9, and hyphen (-). Hyphens are allowed only when theyaposre
-- surrounded by letters, numbers, or other hyphens. You canapost specify a
-- hyphen at the beginning or end of a label. To specify an Internationalized
-- Domain Name, you must convert the name to Punycode.
--
-- Required: Yes
dtfdDomainName :: Lens' DeleteTagsForDomain Text
dtfdDomainName = lens _dtfdDomainName (\s a -> s { _dtfdDomainName = a })

-- | A list of tag keys to delete.
--
-- Type: A list that contains the keys of the tags that you want to delete.
--
-- Default: None
--
-- Required: No
--
-- '>
dtfdTagsToDelete :: Lens' DeleteTagsForDomain [Text]
dtfdTagsToDelete = lens _dtfdTagsToDelete (\s a -> s { _dtfdTagsToDelete = a }) . _List

data DeleteTagsForDomainResponse = DeleteTagsForDomainResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteTagsForDomainResponse' constructor.
deleteTagsForDomainResponse :: DeleteTagsForDomainResponse
deleteTagsForDomainResponse = DeleteTagsForDomainResponse

instance ToPath DeleteTagsForDomain where
    toPath = const "/"

instance ToQuery DeleteTagsForDomain where
    toQuery = const mempty

instance ToHeaders DeleteTagsForDomain

instance ToJSON DeleteTagsForDomain where
    toJSON DeleteTagsForDomain{..} = object
        [ "DomainName"   .= _dtfdDomainName
        , "TagsToDelete" .= _dtfdTagsToDelete
        ]

instance AWSRequest DeleteTagsForDomain where
    type Sv DeleteTagsForDomain = Route53Domains
    type Rs DeleteTagsForDomain = DeleteTagsForDomainResponse

    request  = post "DeleteTagsForDomain"
    response = nullResponse DeleteTagsForDomainResponse
