{-# LANGUAGE TemplateHaskell #-}

-- Module      : Khan.Gen.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Override where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson           (FromJSON)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parse
import           Data.CaseInsensitive (CI)
import           Data.Text            (Text)

data Spec = Spec
    { _specRenameTo   :: Maybe Text             -- ^ Rename type
    , _specReplacedBy :: Maybe Text             -- ^ Existing type that supplants this type
    , _specEnumPrefix :: Maybe Text             -- ^ Enum constructor prefix
    , _specEnumValues :: HashMap Text Text      -- ^ Supplemental sum constructors.
    , _specRequired   :: HashSet (CI Text)      -- ^ Required fields
    , _specOptional   :: HashSet (CI Text)      -- ^ Optional fields
    , _specRenamed    :: HashMap (CI Text) Text -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Spec

data Override = Override
    { _ovUrl              :: Text
    , _ovOperationUrl     :: Text
    , _ovOperationModules :: [Text]
    , _ovTypeModules      :: [Text]
    , _ovOverrides        :: HashMap Text Spec
    , _ovIgnoredWaiters   :: HashSet (CI Text)
    } deriving (Eq, Show)

makeLenses ''Override
