{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Khan.Gen.TH
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.TH
    ( deriveFromJSON

    , Options(..)
    , upper
    , lower
    , spinal
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Jason.TH
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate
import           Khan.Gen.Text
import           Language.Haskell.TH

upper, lower, spinal :: Options
upper  = defaults { constructorTagModifier = asText Text.toUpper }
lower  = defaults { constructorTagModifier = asText Text.toLower }
spinal = defaults

defaults :: Options
defaults = defaultOptions
    { constructorTagModifier = asText toSpinal
    , fieldLabelModifier     = asText stripLens
    , allNullaryToStringTag  = True
    , sumEncoding            =
        defaultTaggedObject
            { tagFieldName      = "type"
            , contentsFieldName = "contents"
            }
    }
