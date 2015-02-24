{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Documentation
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Documentation
    ( Doc
    , format
    , toHaddock
    , toText
    ) where

import           Control.Applicative
import qualified Data.Aeson          as Aeson
import           Data.Default.Class
import           Data.Jason
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Pandoc

newtype Doc = Doc Pandoc
    deriving (Eq)

instance Show Doc where
    show = toHaddock 72

instance FromJSON Doc where
    parseJSON = withText "doc" (pure . format)

instance Aeson.ToJSON Doc where
    toJSON = Aeson.String . Text.pack . toHaddock 76

format :: Text -> Doc
format = Doc . readHtml def . Text.unpack

toHaddock :: Int -> Doc -> String
toHaddock n (Doc p) =
    writeHaddock (def { writerColumns = n, writerWrapText = True }) p

toText :: Int -> Doc -> Text
toText n = Text.pack . toHaddock n
