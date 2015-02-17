{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- Module      : Gen.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor
import           Data.CaseInsensitive             (CI)
import qualified Data.CaseInsensitive             as CI
import           Data.Default.Class
import qualified Data.Foldable                    as Fold
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as Map
import           Data.HashSet                     (HashSet)
import qualified Data.HashSet                     as Set
import           Data.List                        (findIndex)
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Lazy                   as LText
import qualified Data.Text.Lazy.Builder           as Build
import           Data.Text.Manipulate
import           Gen.Model                        hiding (Name, State)
import           Gen.OrdMap                       (OrdMap)
import qualified Gen.OrdMap                       as OrdMap
import           Gen.Text                         (safeHead)
import           Gen.Types
import qualified HIndent                          as HIndent
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Pretty     (prettyPrint)
import qualified Language.Haskell.Stylish         as Style
import           Prelude                          hiding (Enum)

override :: HashMap Text Rules -> HashMap Text Shape -> HashMap Text Shape
override o = Map.foldlWithKey' go mempty
  where
    go acc n = shape (fromMaybe def (Map.lookup n o)) acc n

    shape :: Rules -> HashMap Text Shape -> Text -> Shape -> HashMap Text Shape
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

        requireFields :: Shape -> Shape
        requireFields = _SStruct . structRequired
            %~ (<> _ruleRequired rs)

        optionalFields :: Shape -> Shape
        optionalFields = _SStruct . structRequired
            %~ (`Set.difference` _ruleOptional rs)

        renameFields :: Shape -> Shape
        renameFields = _SStruct . structMembers
            %~ OrdMap.mapWithKey (\k -> (f k,))
          where
            f k = fromMaybe k $ do
                k' <- Map.lookup (k ^. memOriginal) (_ruleRenamed rs)
                return (k & memName .~ k')

        retypeFields :: Shape -> Shape
        retypeFields = references %~ f replacedBy . f renamedTo
          where
            f m v = maybe v (\x -> v & refShape .~ x)
                $ Map.lookup (v ^. refShape) m

        prefixEnum :: Shape -> Shape
        prefixEnum = _SEnum . enumValues %~ f
          where
            f vs = fromMaybe vs $ do
                p <- _ruleEnumPrefix rs
                return . Map.fromList
                       . map (first (p <>))
                       $ Map.toList vs

        appendEnum :: Shape -> Shape
        appendEnum = _SEnum . enumValues %~ mappend (_ruleEnumValues rs)

    renamedTo :: HashMap Text Text
    renamedTo = buildMapping _ruleRenameTo

    replacedBy :: HashMap Text Text
    replacedBy = buildMapping _ruleReplacedBy

    buildMapping :: (Rules -> Maybe Text) -> HashMap Text Text
    buildMapping f = Map.fromList $
        mapMaybe (\(k, v) -> (k,) <$> f v) (Map.toList o)

-- FIXME: How to deal with reserved words? In the prefixing algos?

type PS = HashMap Text (HashSet Text)

prefix :: HashMap Text Shape -> HashMap Text (Prefix Shape)
prefix ss = evalState (Map.traverseWithKey go ss) (mempty, mempty)
  where
    go :: Text -> Shape -> State (PS, PS) (Prefix Shape)
    go n s = Prefix <$> unique n s <*> pure s

    unique :: Text -> Shape -> State (PS, PS) Text
    unique n = \case
        SStruct x -> next _1 (map Text.toLower (heuristics n))
            . Set.fromList
            . map _memName
            . OrdMap.keys
            $ x ^. structMembers
        SEnum   x -> next _2 (heuristics n)
            . Set.fromList
            . Map.keys
            $ x ^. enumValues
        _         -> pure n

    next :: Lens' (PS, PS) PS -> [Text] -> HashSet Text -> State (PS, PS) Text
    next _ []     _  = fail "Unable to calculate prefix"
    next l (x:xs) ks = do
        m <- use l
        case Map.lookup x m of
            Just js | not (Set.null (Set.intersection js ks))
                -> next l xs ks
            _   -> l %= Map.insertWith (<>) x ks >> pure x

    heuristics :: Text -> [Text]
    heuristics (Text.replace ":" "-" -> n) =
        filter ((<= 5) . Text.length) $
            catMaybes
                [ toAcronym n                              -- SomeTestType -> STT
                , Text.toUpper <$> safeHead n              -- SomeTestType -> S
                , upperHead <$> listToMaybe (splitWords n) -- SomeTestType -> Some
                ]

-- data Prefixed = Prefixed
--     { _structPrefixes :: HashMap Text (HashSet Text)
--     , _enumPrefixes   :: HashMap Text (HashSet Text)
--     }

--over traverse :: Traversable t => (a -> b) -> t a -> t b

-- pretty :: (Monad m, MonadError String m, Pretty a) => a -> m LText.Text
-- pretty d = hoist $ HIndent.reformat HIndent.johanTibell Nothing (LText.pack x)
--   where
--     hoist (Left  e) = throwError (e ++ ": ->" ++ x)
--     hoist (Right o) = return (Build.toLazyText o)

--     x = prettyPrintStyleMode style' mode' d

--     style' = style
--         { mode           = PageMode
--         , lineLength     = 80
--         , ribbonsPerLine = 1.5
--         }

--     mode' = defaultMode
--         { spacing = False
--         , layout  = PPNoLayout
--         }

-- When prefixing a structs' members you need to retain the unmodified/original
-- name so the pagination etc lookup can occur

-- -- | Context containing the 'Decl' created from a 'Shape', and the corresponding
-- -- 'Type' which can be used by 'Ref's to point to the correct declaration.
-- type Universe = HashMap Text (Type, Decl)

-- typeOf k = fst `liftM` singleton k
-- declOf k = snd `liftM` singleton k

-- singleton :: (MonadError String m, MonadReader Universe m)
--           => Text
--           -> m (Type, Decl)
-- singleton k = asks (Map.lookup k) >>=
--     maybe (throwError ("Shape doesn't exist: " ++ Text.unpack k)) return

-- -- | Instantiate the type universe.
-- instantiate :: (Monad m, MonadError String m)
--             => HashMap Text Shape
--             -> m Universe
-- instantiate ss = Fold.foldrM go mempty (Map.toList ss)
--   where
--     go (k, v) ts = do
--         d <- declare k v
--         t <- solve d
--         return $! Map.insert k (t, d) ts

-- solve :: MonadError String m => Decl -> m Type
-- solve = TyVar (Ident )

-- -- | Create Haskell AST declaration from a 'Shape'.
-- declare :: (Monad m, MonadError String m) => Text -> Shape -> m Decl
-- declare (Text.unpack -> name) = \case
--     -- SList   x ->
--     -- SMap    x ->
--     SStruct x -> return (struct x)
--     -- SString x ->
--     -- SEnum   x ->
--     -- SBlob   x ->
--     -- SBool   x ->
--     -- STime   x ->
--     -- SInt    x ->
--     -- SDouble x ->
--     -- SLong   x ->
--     _ -> throwError $ "Attempting to solve unsupported type: " ++ name
--   where
--     struct :: Struct -> Decl
--     struct Struct{..} = record (fields _structMembers) (derive ["Eq", "Show"])

--     record :: [([Name], Type)] -> [Deriving] -> Decl
--     record fs = DataDecl l s [] n [] ctor
--       where
--         ctor = [QualConDecl (SrcLoc name 0 10) [] [] (RecDecl n fs)]

--         s | [_] <- fs = NewType
--           | otherwise = DataType

--     fields :: OrdMap Ref -> [([Name], Type)]
--     fields (ordMap -> fs) = map f fs
--       where
--         f (Text.unpack -> k, Text.unpack . _refShape -> v) =
--             ([Ident k], TyCon (UnQual (Ident v)))

--     derive :: [Text] -> [Deriving]
--     derive = map f
--       where
--         f (Text.unpack -> n) = (UnQual (Ident n), [])

--     l = SrcLoc name 0 0
--     n = Ident name

-- -- lens = [sig, pat]
-- --   where
-- --     sig = TypeSig l [Ident l "naeRuleAction"] (TyApp l (TyApp l (TyCon l (UnQual l (Ident l "Lens'"))) (TyCon l (UnQual l (Ident l "NetworkAclEntry")))) (TyParen l (TyApp l (TyCon l (UnQual l (Ident l "Maybe"))) (TyCon l (UnQual l (Ident l "RuleAction"))))))
-- --     pat = PatBind l (PVar l (Ident l "naeRuleAction")) (UnGuardedRhs l (App l (App l (Var l (UnQual l (Ident l "lens"))) (Var l (UnQual l (Ident l "_naeRuleAction")))) (Paren l (Lambda l [PVar l (Ident l "s"),PVar l (Ident l "a")] (RecUpdate l (Var l (UnQual l (Ident l "s"))) [FieldUpdate l (UnQual l (Ident l "_naeRuleAction")) (Var l (UnQual l (Ident l "a")))]))))) Nothing

-- --     l = mempty mempty

