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
    ( deriveJSON
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Jason.TH        hiding (deriveJSON)
import           Data.Text            (Text)
import           Data.Text.Manipulate
import           Khan.Gen.Text
import           Language.Haskell.TH

deriveJSON :: Name -> Q [Dec]
deriveJSON = deriveFromJSON $
    defaultOptions
        { constructorTagModifier = asText toSpinal
        , fieldLabelModifier     = asText stripLens
        , allNullaryToStringTag  = True
        , sumEncoding            =
            defaultTaggedObject
                { tagFieldName      = "type"
                , contentsFieldName = "contents"
                }
        }
