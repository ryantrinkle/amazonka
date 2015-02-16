{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

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
import           Control.Monad.State
import           Data.Bifunctor
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import qualified Data.Foldable                as Fold
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as Set
import           Data.List                    (findIndex)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Gen.Model                    hiding (Name)
import           Gen.OrdMap                   (OrdMap)
import qualified Gen.OrdMap                   as OrdMap
import           Gen.Types
import qualified HIndent                      as HIndent
import           Language.Haskell.Exts
import           Language.Haskell.Exts.Pretty (prettyPrint)
import qualified Language.Haskell.Stylish     as Style
import           Prelude                      hiding (Enum)

-- All renaming, substitution, uniquifying should happen over the original
-- (boto) AST, also the type level overrides, aka Rules.

-- HashMap Text Shape -> HashMap Text Shape
-- ^ fold over the original (k,v) constructing a new hashmap by recursively solving.

override :: HashMap Text Rules -> HashMap Text Shape -> HashMap Text Shape
override o = undefined -- flip (Map.foldlWithKey' go)
  where
    go Rules{..} = undefined
      where
    -- go :: HashMap Text Shape -- ^ Accumulator.
    --    -> Text               -- ^ Shape name.
    --    -> Rules              -- ^ Rules to apply.
    --    -> HashMap Text Shape
    -- go acc k Rules{..} =
    --       renameTo   k (r ^. oRenameTo)
    --     . replacedBy k (r ^. oReplacedBy)
    --     . prefixEnum k (r ^. oSumPrefix)
    --     . appendEnum k (r ^. oSumValues)
    --     . Map.adjust (dataFields %~ fld) k
    --     $ r
    --   where
    --     fld = required (r ^. oRequired) (r ^. oOptional)
    --         . renamed  (r ^. oRenamed)

    --     shape = id
    --     field = id

        -- Take all the rules, and build a list of rename/replace mappings,
        -- and use this in the 'renameFields' step below if more efficient

        prefixEnum :: Enum -> Enum
        prefixEnum e = fromMaybe e $ do
            p <- _ruleEnumPrefix
            return $! e
                & enumValues
                %~ Map.fromList . map (first (p <>)) . Map.toList

        appendEnum :: Enum -> Enum
        appendEnum = enumValues %~ mappend _ruleEnumValues

        requireFields :: Struct -> Struct
        requireFields = structRequired %~ (<> _ruleRequired)

        optionalFields :: Struct -> Struct
        optionalFields = structRequired %~ (`Set.difference` _ruleRequired)

        updateFields :: Struct -> Struct
        updateFields = structMembers %~ OrdMap.map go
          where
            go :: Member -> Ref -> (Member, Ref)
            go k v =
                ( rename k
                , retype replacedBy . retype renamedTo $ v
                )

            rename :: Member -> Member
            rename k = fromMaybe k $ do
                n <- Map.lookup (k ^. memOriginal) _ruleRenamed
                return (k & memName .~ n)

            retype :: HashMap Text Text -> Ref -> Ref
            retype m v = maybe v (\x -> v & refShape .~ x) $
                Map.lookup (v ^. refShape) m

    renamedTo :: HashMap Text Text
    renamedTo = buildMapping _ruleRenameTo

    replacedBy :: HashMap Text Text
    replacedBy = buildMapping _ruleReplacedBy

    buildMapping :: (Rules -> Maybe Text) -> HashMap Text Text
    buildMapping f = Map.fromList $
        mapMaybe (\(k, v) -> (k,) <$> f v) (Map.toList o)

uniquify :: HashMap Text Shape -> HashMap Text Shape
uniquify = undefined

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

