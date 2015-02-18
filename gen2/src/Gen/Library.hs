{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Control.Applicative
import           Control.Error
import           Control.Lens              ((^.))
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO.Class
import qualified Data.Aeson                as A
import           Data.Aeson.Encode.Pretty
import           Data.Bifunctor
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.List                 (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.JSON
import           Gen.Model
import           Gen.Types
import           Prelude                   hiding (FilePath)
import           System.Directory.Tree     hiding (dir, file)
import           Text.EDE                  (Template)
import qualified Text.EDE                  as EDE

-- Maybe the dirtree could be paramterized over a conduit and the assets
-- could just be copied via that?

-- Printing/logging can be put into the a of DirTree a also

-- tree :: FilePath
--      -> Template Protocol
--      -> Service (Prefix Shape)
--      -> DirTree String
tree d _ s = encodeString d :/ dir lib
    [ dir "src" []
    , dir "examples"
        [ dir "src" []
        , file (fromText $ library <> "-examples.cabal") cabalExample
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
    , file "Makefile" makefile
    , file "README.md" readme
    ]
  where
    lib     = fromText library
    library = s ^. svcLibrary
    abbrev  = fromText (s ^. svcAbbrev)

    -- How to handle the rendering of these with the Either result?
    -- Maybe they file contents is also the Conduit parameterised over failure?
    cabal    = ""
    makefile = ""
    readme   = ""

    cabalExample    = ""
    makefileExample = ""

    service = ""
    types   = ""
    waiters = ""

    operations = []

(<//>) :: FilePath -> DirTree a -> DirTree a
p <//> d = dir p [d]

dir :: FilePath -> [DirTree a] -> DirTree a
dir p = Dir (encodeString p)

file :: FilePath -> a -> DirTree a
file p = File (encodeString p)
