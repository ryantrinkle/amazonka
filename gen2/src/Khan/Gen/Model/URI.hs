{-# LANGUAGE TemplateHaskell #-}

-- Module      : Khan.Gen.Model.URI
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Model.URI where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson           (FromJSON)
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as Parse
import           Data.Text            (Text)

data Segment
    = SVar   Text
    | SConst Text
      deriving (Eq)

makePrisms ''Segment

data URI = URI
    { _uriPath  :: [Segment]
    , _uriQuery :: [Segment]
    } deriving (Eq)

makeLenses ''URI

uriSegments :: Traversal' URI Segment
uriSegments f x = URI <$> traverse f (_uriPath x) <*> traverse f (_uriQuery x)

uriVariables :: Traversal' URI Text
uriVariables = uriSegments . _SVar

uriParser :: Parser URI
uriParser = URI
    <$> some seg
    <*> Parse.option [] (Parse.char '?' *> some seg)
    <*  Parse.endOfInput
  where
    seg = SConst <$> Parse.takeWhile1 (end '{')
      <|> SVar <$> var

    var = Parse.char '{' *> Parse.takeWhile1 (end '}') <* Parse.char '}'

    end x y | x == y = False
    end _ '?'        = False
    end _ _          = True
