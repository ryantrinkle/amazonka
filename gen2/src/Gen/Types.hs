{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- Module      : Gen.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text      (Parser)
import qualified Data.Attoparsec.Text      as Parse
import           Data.Bifunctor
import           Data.CaseInsensitive      (CI)
import qualified Data.CaseInsensitive      as CI
import           Data.Default.Class
import           Data.Foldable             (Foldable)
import           Data.Function             (on)
import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.HashSet              (HashSet)
import           Data.Jason.Types
import           Data.Monoid
import           Data.SemVer               (Version, fromText, toText)
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           Data.Traversable          (Traversable)
import qualified Filesystem.Path.CurrentOS as Path
import           Gen.TH
import           GHC.Generics              (Generic)
import           GHC.TypeLits
import           Text.EDE                  (Template)

encode :: Path.FilePath -> Text
encode = either id id . Path.toText

data Prefix a = Prefix
    { _prefKey  :: Text
    , _prefItem :: a
    } deriving (Eq, Show)

makeLenses ''Prefix

data Member = Member
    { _memOriginal :: Text
    , _memName     :: Text
    } deriving (Show, Generic)

instance Eq Member where
    (==) = on (==) _memName

instance Hashable Member

makeLenses ''Member

data Rules = Rules
    { _ruleRenameTo   :: Maybe Text             -- ^ Rename type
    , _ruleReplacedBy :: Maybe Text             -- ^ Existing type that supplants this type
    , _ruleEnumPrefix :: Maybe Text             -- ^ Enum constructor prefix
    , _ruleEnumValues :: HashMap Text Text      -- ^ Supplemental sum constructors.
    , _ruleRequired   :: HashSet (CI Text)      -- ^ Required fields
    , _ruleOptional   :: HashSet (CI Text)      -- ^ Optional fields
    , _ruleRenamed    :: HashMap (CI Text) Text -- ^ Rename fields
    } deriving (Eq, Show)

makeLenses ''Rules

instance FromJSON Rules where
    parseJSON = withObject "rules" $ \o -> Rules
        <$> o .:? "renameTo"
        <*> o .:? "replacedBy"
        <*> o .:? "enumPrefix"
        <*> o .:? "enumValues" .!= mempty
        <*> o .:? "required"   .!= mempty
        <*> o .:? "optional"   .!= mempty
        <*> o .:? "renamed"    .!= mempty

instance Default Rules where
    def = Rules Nothing Nothing Nothing mempty mempty mempty mempty

data Override = Override
    { _ovOperationImports :: [Text]
    , _ovTypeImports      :: [Text]
    , _ovIgnoredWaiters   :: HashSet (CI Text)
    , _ovOverrides        :: HashMap Text Rules
    } deriving (Eq, Show)

makeClassy ''Override

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "operationImports" .!= mempty
        <*> o .:? "typeImports"      .!= mempty
        <*> o .:? "ignoredWaiters"   .!= mempty
        <*> o .:? "overrides"        .!= mempty

data Templates a = Templates
    { _tmplCabal           :: Template
    , _tmplService         :: Template
    , _tmplWaiters         :: Template
    , _tmplReadme          :: Template
    , _tmplCabalExample    :: Template
    , _tmplMakefileExample :: Template
    , _tmplSelect          :: a -> (Template, Template)
    }

makeLenses ''Templates

tmplTypes, tmplOperation :: Getter (Templates a) (a -> Template)
tmplTypes     = to (\s -> fst . _tmplSelect s)
tmplOperation = to (\s -> snd . _tmplSelect s)

instance FromJSON (CI Text) where
    parseJSON = withText "ci" (return . CI.mk)

instance FromJSON a => FromJSON (HashMap (CI Text) a) where
    parseJSON = fmap (Map.fromList . map (first CI.mk) . Map.toList) . parseJSON

instance FromJSON Version where
    parseJSON = withText "semantic_version" $
        either fail return . fromText
