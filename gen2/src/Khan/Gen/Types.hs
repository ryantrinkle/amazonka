{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

-- Module      : Khan.Gen.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Types where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson                (FromJSON)
import           Data.Attoparsec.Text      (Parser)
import qualified Data.Attoparsec.Text      as Parse
import           Data.CaseInsensitive      (CI)
import           Data.Foldable             (Foldable)
import           Data.HashMap.Strict       (HashMap)
import           Data.HashSet              (HashSet)
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           Data.Text                 (Text)
import           Data.Traversable          (Traversable)
import qualified Filesystem.Path.CurrentOS as Path
import           GHC.TypeLits
import           Text.EDE                  (Template)

newtype Path (a :: Symbol) = Path { toFilePath :: Path.FilePath }
    deriving (Show, IsString)

type OutputDir   = Path "output"
type TemplateDir = Path "template"
type OverrideDir = Path "override"
type AssetDir    = Path "asset"

type ModelPath  = Path "model"
type RetryPath  = Path "retry"
type WaiterPath = Path "waiter"
type PagerPath  = Path "pager"

data OrdMap a = OrdMap { ordMap :: [(Text, a)] }
    deriving (Eq, Functor, Foldable, Traversable)

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

data Override = Override
    { _ovUrl              :: Text
    , _ovOperationUrl     :: Text
    , _ovOperationModules :: [Text]
    , _ovTypeModules      :: [Text]
    , _ovOverrides        :: HashMap Text Rules
    , _ovIgnoredWaiters   :: HashSet (CI Text)
    } deriving (Eq, Show)

makeLenses ''Override

data Templates a = Templates
    { _tmplCabal           :: Template
    , _tmplService         :: Template
    , _tmplWaiters         :: Template
    , _tmplReadme          :: Template
    , _tmplCabalExample    :: Template
    , _tmplMakefileExample :: Template
    , _tmplSelect          :: a -> (Template, Template)
    }

makeLenses ''Template
