{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Gen.Override
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Override where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Default.Class
import qualified Data.Foldable                as Fold
import qualified Data.HashMap.Strict          as Map
import qualified Data.HashSet                 as Set
import           Data.List                    (intercalate)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Text.Manipulate
import           Data.Traversable             (traverse)
import qualified Gen.AST                      as AST
import           Gen.Model                    hiding (Name, State)
import qualified Gen.OrdMap                   as OrdMap
import           Gen.Text                     (safeHead)
import           Gen.Types                    hiding (override)
import           Language.Haskell.Exts.Syntax (Type)

type PS = Map.HashMap (CI Text) (Set.HashSet (CI Text))

type Untyped (f :: * -> *) = f Text
type Typed   (f :: * -> *) = f Type

type HashMap = Map.HashMap Text
type HashSet = Set.HashSet Text

-- FIXME: Need to deal with shared request/response types, do the renaming of
-- said input/outputs, and remove them from the shapes list if necessary.

-- service :: (Functor m, MonadError String m)
--         => Service (Shape Text) (Ref Text)
--         -> m (Service (Prefix (Shape Type)) (Ref Text))
-- service s = do
--     x <- prefix . primitives $
--         override (s ^. svcOverride . ovOverrides) (s ^. svcShapes)
--     return $! s & svcShapes .~ x

-- | Replace the untyped 'Text' references with actual Haskell 'Type's.
solve :: (Functor m, MonadError String m)
      => HashMap (Untyped Shape)
      -> m (HashMap (Typed Shape))
solve ss = evalStateT (traverse (traverseOf references go) ss) mempty
  where
    go :: MonadError String m
       => Untyped Ref
       -> StateT (HashMap Type) m (Typed Ref)
    go r = flip (set refShape) r `liftM` memo (r ^. refShape)

    memo :: MonadError String m
         => Text
         -> StateT (HashMap Type) m Type
    memo n = do
        m <- gets (Map.lookup n)
        case m of
            Just t  -> return t
            Nothing ->
                case Map.lookup n ss of
                    Just (typeOf n -> t) -> modify (Map.insert n t) >> return t
                    Nothing              -> throwError $ "Missing Shape " ++ show n

    typeOf :: Text -> Untyped Shape -> Type
    typeOf n = \case
        SStruct x -> AST.tyCon n
        SList   x -> list x
        SMap    x -> hmap x
        SBlob   x -> stream x
        SBool   x -> AST.tyCon "Bool"
        -- FIXME: This is dependent on the service.
        STime   x -> time x
        SDouble _ -> AST.tyCon "Double"
        SInt    x -> natural x "Int"
        SLong   x -> natural x "Integer"
      where
        list = undefined -- (List e a) || (List1 e a)

        hmap = undefined -- (Map k v) || (EMap e i j k v)

        stream = undefined -- figure out streaming or not

        time = AST.tyCon
             . Text.pack
             . show
             . fromMaybe RFC822
             . view timeTimestampFormat

        natural x
            | x ^. numMin > Just 0 = const (AST.tyCon "Natural")
            | otherwise            = AST.tyCon

-- -- | Assign unique prefixes to 'Enum' and 'Struct' shapes.
-- prefix :: (Functor m, MonadError String m)
--        => HashMap (Pre (Untyped Shape))
--        -> m (Map (Pre (Untyped Shape)))
-- prefix ss = evalStateT (Map.traverseWithKey go ss) (mempty, mempty)
--   where
--     go :: (Functor m, MonadError String m)
--        => Text
--        -> Shape
--        -> StateT (PS, PS) m (Pre (Untyped Shape))
--     go n s = Prefix <$> unique n s <*> pure s

--     unique :: (Functor m, MonadError String m)
--            => Text
--            -> Shape
--            -> StateT (PS, PS) m Text
--     unique n = \case
--         SStruct x -> next n _1 (heuristics n)
--             . Set.fromList
--             . map (CI.mk . _memName)
--             . OrdMap.keys
--             $ x ^. structMembers
--         SEnum   x -> next n _2 (mempty : heuristics n)
--             . Set.fromList
--             . map CI.mk
--             . Map.keys
--             $ x ^. enumValues
--         _         -> pure n

--     next :: (Functor m, MonadError String m)
--          => Text
--          -> Lens' (PS, PS) PS
--          -> [CI Text]
--          -> HashSet (CI Text)
--          -> StateT (PS, PS) m Text
--     next k l []     ks = do
--         m <- use l
--         throwError . intercalate "\n" $
--               ("Error selecting prefix for: " <> Text.unpack k)
--             : ("Fields: " <> show ks)
--             : map (\h -> show h <> " => " <> show (Map.lookup h m)) (heuristics k)
--     next k l (x:xs) ks = do
--         m <- use l
--         case Map.lookup x m of
--             Just js | not (Set.null (Set.intersection js ks))
--                 -> next k l xs ks
--             _   -> l %= Map.insertWith (<>) x ks >> pure (CI.original x)

--     heuristics :: Text -> [CI Text]
--     heuristics n = rules ++ ordinal
--       where
--         -- Acronym preference list.
--         rules = map CI.mk $ catMaybes [r1, r2, r3, r4]

--         -- SomeTestType -> STT
--         r1 = toAcronym n

--         -- SomeTestType -> S
--         r3 = Text.toUpper <$> safeHead n

--         -- Some -> Some || SomeTestType -> Some
--         r2 | Text.length n <= 3 = Just n
--            | otherwise          = Just (Text.take 3 n)

--         -- SomeTestType -> Som
--         r4 = upperHead <$> listToMaybe (splitWords n)

--         -- Append an ordinal to the generated acronyms.
--         ordinal = concatMap (\i -> map (\x -> mappend x (num i)) rules) [1..3]

--         num :: Int -> CI Text
--         num = CI.mk . Text.pack . show

-- | Apply the override rulset to shapes and their respective fields.
override :: HashMap Rules -> HashMap (Shape Text) -> HashMap (Shape Text)
override o = Map.foldlWithKey' go mempty
  where
    go acc n = shape (fromMaybe def (Map.lookup n o)) acc n

    shape :: Rules
          -> HashMap (Shape Text)
          -> Text
          -> (Shape Text)
          -> HashMap (Shape Text)
    shape rs acc n s
        | Map.member n replacedBy          = acc
        | Just x <- Map.lookup n renamedTo = shape rs acc x s
        | otherwise                        = Map.insert n (rules s) acc
      where
        rules = requireFields
              . optionalFields
              . renameFields
              . retypeFields
              . prefixEnum
              . appendEnum

        requireFields :: Shape Text -> Shape Text
        requireFields = _SStruct . structRequired
            %~ (<> _ruleRequired rs)

        optionalFields = _SStruct . structRequired
            %~ (`Set.difference` _ruleOptional rs)

        renameFields :: Shape Text -> Shape Text
        renameFields = _SStruct . structMembers
            %~ OrdMap.mapWithKey (\k -> (f k,))
          where
            f k = fromMaybe k $ do
                k' <- Map.lookup (CI.mk (k ^. memOriginal)) (_ruleRenamed rs)
                return (k & memName .~ k')

        retypeFields :: Shape Text -> Shape Text
        retypeFields = references %~ f replacedBy . f renamedTo
          where
            f m v = maybe v (\x -> v & refShape .~ x)
                $ Map.lookup (v ^. refShape) m

        prefixEnum :: Shape Text -> Shape Text
        prefixEnum = _SEnum . enumValues %~ f
          where
            f vs = fromMaybe vs $ do
                p <- _ruleEnumPrefix rs
                return . Map.fromList
                       . map (first (p <>))
                       $ Map.toList vs

        appendEnum :: Shape Text -> Shape Text
        appendEnum = _SEnum . enumValues %~ mappend (_ruleEnumValues rs)

    renamedTo :: HashMap Text
    renamedTo = buildMapping _ruleRenameTo

    replacedBy :: HashMap Text
    replacedBy = buildMapping _ruleReplacedBy

    buildMapping :: (Rules -> Maybe Text) -> HashMap Text
    buildMapping f = Map.fromList $
        mapMaybe (\(k, v) -> (k,) <$> f v) (Map.toList o)

