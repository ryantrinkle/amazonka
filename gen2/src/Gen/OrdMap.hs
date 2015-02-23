{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Gen.OrdMap
    ( OrdMap
    , toList
    , fromList
    , keys
    , values
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Bifunctor
import           Data.Foldable       (Foldable)
import           Data.Jason.Types
import           Data.Monoid
import           Data.Text           (Text)
import           Prelude             hiding (map)

data OrdMap k v = OrdMap { toList :: [(k, v)] }
    deriving (Eq, Functor, Foldable, Traversable, Show)

instance Monoid (OrdMap k v) where
    mempty      = OrdMap mempty
    mappend a b = OrdMap (toList a <> toList b)

instance FromJSON v => FromJSON (OrdMap Text v) where
    parseJSON = withObject "ordered_map" $ \(unObject -> o) ->
        OrdMap <$> traverse (\(k, v) -> (k,) <$> parseJSON v) o

instance Bifunctor OrdMap where
    bimap f g = OrdMap . fmap (bimap f g) . toList

fromList :: [(k, v)] -> OrdMap k v
fromList = OrdMap

keys :: OrdMap k v -> [k]
keys = fmap fst . toList

values :: OrdMap k v -> [v]
values = fmap snd . toList
