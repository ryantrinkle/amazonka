{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Gen.OrdMap
    ( OrdMap
    , toList
    , fromList
    , map
    , mapWithKey
    , keys
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Attoparsec.Text      (Parser)
import qualified Data.Attoparsec.Text      as Parse
import           Data.Bifunctor
import           Data.Bifunctor
import           Data.CaseInsensitive      (CI)
import qualified Data.CaseInsensitive      as CI
import           Data.Foldable             (Foldable)
import           Data.Hashable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as Map
import           Data.HashSet              (HashSet)
import qualified Data.HashSet              as Set
import           Data.Jason.Types
import           Data.Monoid
import           Data.SemVer               (Version, fromText, toText)
import           Data.String               (IsString)
import           Data.Text                 (Text)
import           Data.Traversable          (Traversable)
import qualified Filesystem.Path.CurrentOS as Path
import           Gen.TH
import           GHC.TypeLits
import           Prelude                   hiding (map)
import           Text.EDE                  (Template)

data OrdMap k v = OrdMap { toList :: [(k, v)] }
    deriving (Eq, Functor, Foldable, Traversable, Show)

instance Monoid (OrdMap k v) where
    mempty      = OrdMap mempty
    mappend a b = OrdMap (toList a <> toList b)

instance FromJSON v => FromJSON (OrdMap Text v) where
    parseJSON = withObject "ordered_map" $ \(unObject -> o) ->
        OrdMap <$> traverse (\(k, v) -> (k,) <$> parseJSON v) o

fromList :: [(k, v)] -> OrdMap k v
fromList = OrdMap

map :: (v -> v') -> OrdMap k v -> OrdMap k v'
map f = mapWithKey (\k v -> (k, f v))

mapWithKey :: (k -> v -> (k', v')) -> OrdMap k v -> OrdMap k' v'
mapWithKey f = OrdMap . fmap (uncurry f) . toList

keys :: OrdMap k v -> [k]
keys = fmap fst . toList
