{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Filters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Filters where

import           Data.Char
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as Map
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Manipulate
import           Gen.Documentation
import           Text.EDE.Filters

genFilters :: HashMap Text Term
genFilters = Map.fromList
    [ "indent"       @: indent
    , "highlight"    @: highlightType
    , "parens"       @: parens
    , "wrapped"      @: wrapped
    , "concat"       @: (mappend :: Text -> Text -> Text)
    , "joinedLength" @: joinedLength
    , "member"       @: (elem :: Text -> [Text] -> Bool)
    , "waiter"       @: waiter
    ]

indent :: Text -> Int -> Text
indent t n = Text.unlines . map (sep <>) $ Text.lines t
  where
    sep = Text.replicate n (Text.singleton ' ')

parens :: Text -> Text
parens t = "(" <> t <> ")"

wrapped :: Text -> Text
wrapped t
    | Text.null t        = t
    | Text.head t == '[' = t
    | otherwise          = maybe t (const (parens t)) (Text.findIndex isSpace t)

joinedLength :: [Text] -> Text -> Int
joinedLength xs sep = sum (map ((+n) . Text.length) xs)
  where
    n = Text.length sep

waiter :: Text -> Text
waiter t
    | p "DB"    = "db" <> Text.drop 2 t
    | otherwise = toCamel t
  where
    p = flip Text.isPrefixOf t

