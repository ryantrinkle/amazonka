{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Gen.Library
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Library where

import           Control.Error
import           Control.Lens              (view, (^.))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Char
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.Lazy            as LText
import           Data.Text.Manipulate
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.Documentation         as Doc
import           Gen.Model
import           Gen.Text
import           Gen.Types
import           Prelude                   hiding (FilePath)
import           System.Directory.Tree     hiding (file)
import qualified Text.EDE                  as EDE
import           Text.EDE.Filters

-- data Indent = Indent Int Text

-- instance ToJSON Indent where
--     toJSON (Indent n t) = String (indent n t)

data Cabal a = Cabal SemVer.Version a

instance ToJSON (Cabal (Service a b)) where
    toJSON (Cabal v s) = object
        [ "name"             .= nameToText (s ^. metaServiceFullName)
        , "library"          .= view svcLibrary s
        , "documentation"    .= view svcDocumentation s
        , "documentationUrl" .= view svcDocumentationUrl s
        , "version"          .= SemVer.toText v
        ]

tree :: FilePath
     -> Templates Protocol
     -> SemVer.Version
     -> Service a b
     -> AnchoredDirTree (Either String LText.Text)
tree d t v s = encodeString d :/ dir lib
    [ dir "src" []
    -- , dir "examples"
    --     [ dir "src" []
    --     , file (fromText $ s ^. svcLibrary <> "-examples.cabal") cabalExample
    --     , file "Makefile" makefileExample
    --     ]
    -- , dir "gen"
    --     [ dir "Network"
    --         [ dir "AWS"
    --             [ dir abbrev $
    --                 [ file "Types.hs" types
    --                 , file "Waiters.hs" waiters
    --                 ] ++ map (uncurry file) operations
    --             , file (abbrev <.> "hs") service
    --             ]
    --         ]
    --     ]
    , file (lib <.> "cabal") cabal
    , file "README.md" readme
    ]
  where
    cabal           = render (t ^. tmplCabal)           cbl
    readme          = render (t ^. tmplReadme)          cbl

    cabalExample    = render (t ^. tmplCabalExample)    test
    makefileExample = render (t ^. tmplMakefileExample) test

    service         = render (t ^. tmplService)         test
    types           = render (t ^. tmplTypes $ proto)   test
    waiters         = render (t ^. tmplWaiters)         test


    cbl  = toJSON (Cabal v s)
    test = toJSON Null

--    operations      = map f . Map.toList $ s ^. svcOperations
      -- where
      --   f (k, _) = (fromText k <.> "hs", EDE.eitherRender tmpl mempty)

    -- tmpl = t ^. tmplOperation $ proto

    proto  = s ^. metaProtocol

    abbrev = fromText (s ^. svcAbbrev)
    lib    = fromText (s ^. svcLibrary)

render :: EDE.Template -> Value -> Either String LText.Text
render x = note "Error serialising template params." . EDE.fromValue
    >=> EDE.eitherRenderWith filters x
  where
    filters = Map.fromList
        [ "indent" @: flip indent
        -- , "highlight"    @: highlightType
        -- , "parens"       @: parens
        -- , "wrapped"      @: wrapped
        -- , "concat"       @: (mappend :: Text -> Text -> Text)
        -- , "joinedLength" @: joinedLength
        -- , "member"       @: (elem :: Text -> [Text] -> Bool)
        -- , "waiter"       @: waiter
        ]

root :: AnchoredDirTree a -> FilePath
root (p :/ d) = decodeString p </> decodeString (name d)

(<//>) :: FilePath -> DirTree a -> DirTree a
p <//> d = dir p [d]

dir :: FilePath -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

file :: FilePath -> a -> DirTree a
file p = File (encodeString p)

-- parens :: Text -> Text
-- parens t = "(" <> t <> ")"

-- wrapped :: Text -> Text
-- wrapped t
--     | Text.null t        = t
--     | Text.head t == '[' = t
--     | otherwise          = maybe t (const (parens t)) (Text.findIndex isSpace t)

-- joinedLength :: [Text] -> Text -> Int
-- joinedLength xs sep = sum (map ((+n) . Text.length) xs)
--   where
--     n = Text.length sep

-- waiter :: Text -> Text
-- waiter t
--     | p "DB"    = "db" <> Text.drop 2 t
--     | otherwise = toCamel t
--   where
--     p = flip Text.isPrefixOf t

