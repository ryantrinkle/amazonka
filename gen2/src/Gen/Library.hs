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

import           Control.Lens              ((^.))
import           Data.Aeson.Encode.Pretty
import qualified Data.HashMap.Strict       as Map
import           Data.Monoid
import qualified Data.Text.Lazy            as LText
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.Model
import           Gen.Types
import           Prelude                   hiding (FilePath)
import           System.Directory.Tree     hiding (file)
import qualified Text.EDE                  as EDE

tree :: FilePath
     -> Templates Protocol
     -> Service s
     -> AnchoredDirTree (Either String LText.Text)
tree d t s = encodeString d :/ dir lib
    [ dir "src" []
    , dir "examples"
        [ dir "src" []
        , file (fromText $ s ^. svcLibrary <> "-examples.cabal") cabalExample
        , file "Makefile" makefileExample
        ]
    , dir "gen"
        [ dir "Network"
            [ dir "AWS"
                [ dir abbrev $
                    [ file "Types.hs" types
                    , file "Waiters.hs" waiters
                    ] ++ map (uncurry file) operations
                , file (abbrev <.> "hs") service
                ]
            ]
        ]
    , file (lib <.> "cabal") cabal
    , file "README.md" readme
    ]
  where
    cabal           = EDE.eitherRender (t ^. tmplCabal)           mempty
    readme          = EDE.eitherRender (t ^. tmplReadme)          mempty

    cabalExample    = EDE.eitherRender (t ^. tmplCabalExample)    mempty
    makefileExample = EDE.eitherRender (t ^. tmplMakefileExample) mempty

    service         = EDE.eitherRender (t ^. tmplService)         mempty
    types           = EDE.eitherRender (t ^. tmplTypes $ proto)   mempty
    waiters         = EDE.eitherRender (t ^. tmplWaiters)         mempty

    operations      = map f . Map.toList $ s ^. svcOperations
      where
        f (k, v) = (fromText k <.> "hs", EDE.eitherRender tmpl mempty)

    tmpl = t ^. tmplOperation $ proto

    proto  = s ^. metaProtocol

    abbrev = fromText (s ^. svcAbbrev)
    lib    = fromText (s ^. svcLibrary)

root :: AnchoredDirTree a -> FilePath
root (p :/ d) = decodeString p </> decodeString (name d)

(<//>) :: FilePath -> DirTree a -> DirTree a
p <//> d = dir p [d]

dir :: FilePath -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

file :: FilePath -> a -> DirTree a
file p = File (encodeString p)
