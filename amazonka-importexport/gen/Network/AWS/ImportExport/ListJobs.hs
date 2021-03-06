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

-- Module      : Network.AWS.ImportExport.ListJobs
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

-- | This operation returns the jobs associated with the requester. AWS
-- Import/Export lists the jobs in reverse chronological order based on the date
-- of creation. For example if Job Test1 was created 2009Dec30 and Test2 was
-- created 2010Feb05, the ListJobs operation would return Test2 followed by
-- Test1.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebListJobs.html>
module Network.AWS.ImportExport.ListJobs
    (
    -- * Request
      ListJobs
    -- ** Request constructor
    , listJobs
    -- ** Request lenses
    , ljAPIVersion
    , ljMarker
    , ljMaxJobs

    -- * Response
    , ListJobsResponse
    -- ** Response constructor
    , listJobsResponse
    -- ** Response lenses
    , ljrIsTruncated
    , ljrJobs
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ImportExport.Types
import qualified GHC.Exts

data ListJobs = ListJobs
    { _ljAPIVersion :: Maybe Text
    , _ljMarker     :: Maybe Text
    , _ljMaxJobs    :: Maybe Int
    } deriving (Eq, Ord, Read, Show)

-- | 'ListJobs' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljAPIVersion' @::@ 'Maybe' 'Text'
--
-- * 'ljMarker' @::@ 'Maybe' 'Text'
--
-- * 'ljMaxJobs' @::@ 'Maybe' 'Int'
--
listJobs :: ListJobs
listJobs = ListJobs
    { _ljMaxJobs    = Nothing
    , _ljMarker     = Nothing
    , _ljAPIVersion = Nothing
    }

ljAPIVersion :: Lens' ListJobs (Maybe Text)
ljAPIVersion = lens _ljAPIVersion (\s a -> s { _ljAPIVersion = a })

ljMarker :: Lens' ListJobs (Maybe Text)
ljMarker = lens _ljMarker (\s a -> s { _ljMarker = a })

ljMaxJobs :: Lens' ListJobs (Maybe Int)
ljMaxJobs = lens _ljMaxJobs (\s a -> s { _ljMaxJobs = a })

data ListJobsResponse = ListJobsResponse
    { _ljrIsTruncated :: Maybe Bool
    , _ljrJobs        :: List "member" Job
    } deriving (Eq, Read, Show)

-- | 'ListJobsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ljrIsTruncated' @::@ 'Maybe' 'Bool'
--
-- * 'ljrJobs' @::@ ['Job']
--
listJobsResponse :: ListJobsResponse
listJobsResponse = ListJobsResponse
    { _ljrJobs        = mempty
    , _ljrIsTruncated = Nothing
    }

ljrIsTruncated :: Lens' ListJobsResponse (Maybe Bool)
ljrIsTruncated = lens _ljrIsTruncated (\s a -> s { _ljrIsTruncated = a })

ljrJobs :: Lens' ListJobsResponse [Job]
ljrJobs = lens _ljrJobs (\s a -> s { _ljrJobs = a }) . _List

instance ToPath ListJobs where
    toPath = const "/"

instance ToQuery ListJobs where
    toQuery ListJobs{..} = mconcat
        [ "APIVersion" =? _ljAPIVersion
        , "Marker"     =? _ljMarker
        , "MaxJobs"    =? _ljMaxJobs
        ]

instance ToHeaders ListJobs

instance AWSRequest ListJobs where
    type Sv ListJobs = ImportExport
    type Rs ListJobs = ListJobsResponse

    request  = post "ListJobs"
    response = xmlResponse

instance FromXML ListJobsResponse where
    parseXML = withElement "ListJobsResult" $ \x -> ListJobsResponse
        <$> x .@? "IsTruncated"
        <*> x .@? "Jobs" .!@ mempty

instance AWSPager ListJobs where
    page rq rs
        | stop (rs ^. ljrIsTruncated) = Nothing
        | otherwise = Just $ rq
            & ljMarker .~ rs ^. index ljrJobs jobJobId
