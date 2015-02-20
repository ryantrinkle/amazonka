{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

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
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Default.Class
import qualified Data.Foldable              as Fold
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as Set
import           Data.List                  (intercalate)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Text.Manipulate
import           Data.Traversable           (traverse)
import qualified Gen.AST                    as AST
import           Gen.Model                  hiding (Name, State)
import qualified Gen.OrdMap                 as OrdMap
import           Gen.Text                   (safeHead)
import           Gen.Types                  hiding (override)

type PS = HashMap (CI Text) (HashSet (CI Text))

-- FIXME: Need to deal with shared request/response types, do the renaming of
-- said input/outputs, and remove them from the shapes list if necessary.

-- Leave the request/response types in the Types module, use the Refs as intended


-- * Correct usage of Refs to primitive types (String -> Text) etc.
-- * Remove corrected types from the Shapes map.

service :: (Functor m, MonadError String m)
        => Service (Shape Text) (Ref Text)
        -> m (Service (Prefix (Shape Type)) (Ref Text))
service s = do
    x <- prefix . primitives $
        override (s ^. svcOverride . ovOverrides) (s ^. svcShapes)
    return $! s & svcShapes .~ x

-- Would create a Text->Type index be useful? This could be turned into an actual
-- AST.Type later

-- primitives :: HashMap Text Shape -> HashMap Text Shape
primitives = uncurry (Map.foldlWithKey' go) . join (,)
  where
--    go :: HashMap Text Shape -> Text -> Shape -> HashMap Text Shape
    go acc n s = Map.insert n (s & references %~ ref) acc
      where
        -- use state instead of fold with the actual state being the result.

  --      ref :: Ref -> m Ref
        ref r = do
            -- 1.  lookup the actual shape in the _original_ map, since the
            --     state might not contain the shape.
            -- 1a. error if the shape does not exist.
            -- 1b. apply 'subst' which returns (Either Type Type),
            --     if, then delete from the state.

            -- 2.  return the updated reference.
            undefined

-- -- | Determine sharing of request and response types, and transform the respective
-- -- refs into a whole shape.
-- sharing :: HashMap Text (Operation Ref)
--         -> HashMap Text (Prefix Shape)
--         -> HashMap (Operation (Prefix Shape))
-- sharing os ss = traverse go os

--   where
--     go o = do
--         rq <- ref (o ^. operInput)
--         rs <- ref (o ^. operOutput)
--         return $!
--              o & operInput  ?~ rq
--                & operOutput ?~ rs

--     ref Nothing  = pure placeholder
--     ref (Just r) = do
--         Map.lookup

-- Leave the request/response types in the Types module?

--     placeholder = SStruct $ Struct
--         { _structDocumentation = Nothing
--         , _structRequired      = mempty
--         , _structPayload       = Nothing
--         , _structXmlNamespace  = Nothing
--         , _structException     = Nothing
--         , _structFault         = Nothing
--         , _structMembers       = mempty
--         }

-- | Assign unique prefixes to 'Enum' and 'Struct' shapes.
prefix :: (Functor m, MonadError String m)
       => HashMap Text Shape
       -> m (HashMap Text (Prefix Shape))
prefix ss = evalStateT (Map.traverseWithKey go ss) (mempty, mempty)
  where
    go :: (Functor m, MonadError String m)
       => Text
       -> Shape
       -> StateT (PS, PS) m (Prefix Shape)
    go n s = Prefix <$> unique n s <*> pure s

    unique :: (Functor m, MonadError String m)
           => Text
           -> Shape
           -> StateT (PS, PS) m Text
    unique n = \case
        SStruct x -> next n _1 (heuristics n)
            . Set.fromList
            . map (CI.mk . _memName)
            . OrdMap.keys
            $ x ^. structMembers
        SEnum   x -> next n _2 (mempty : heuristics n)
            . Set.fromList
            . map CI.mk
            . Map.keys
            $ x ^. enumValues
        _         -> pure n

    next :: (Functor m, MonadError String m)
         => Text
         -> Lens' (PS, PS) PS
         -> [CI Text]
         -> HashSet (CI Text)
         -> StateT (PS, PS) m Text
    next k l []     ks = do
        m <- use l
        throwError . intercalate "\n" $
              ("Error selecting prefix for: " <> Text.unpack k)
            : ("Fields: " <> show ks)
            : map (\h -> show h <> " => " <> show (Map.lookup h m)) (heuristics k)
    next k l (x:xs) ks = do
        m <- use l
        case Map.lookup x m of
            Just js | not (Set.null (Set.intersection js ks))
                -> next k l xs ks
            _   -> l %= Map.insertWith (<>) x ks >> pure (CI.original x)

    heuristics :: Text -> [CI Text]
    heuristics n = rules ++ ordinal
      where
        -- Acronym preference list.
        rules = map CI.mk $ catMaybes [r1, r2, r3, r4]

        -- SomeTestType -> STT
        r1 = toAcronym n

        -- SomeTestType -> S
        r3 = Text.toUpper <$> safeHead n

        -- Some -> Some || SomeTestType -> Some
        r2 | Text.length n <= 3 = Just n
           | otherwise          = Just (Text.take 3 n)

        -- SomeTestType -> Som
        r4 = upperHead <$> listToMaybe (splitWords n)

        -- Append an ordinal to the generated acronyms.
        ordinal = concatMap (\i -> map (\x -> mappend x (num i)) rules) [1..3]

        num :: Int -> CI Text
        num = CI.mk . Text.pack . show

-- | Apply the override rulset to shapes and their respective fields.
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
                k' <- Map.lookup (CI.mk (k ^. memOriginal)) (_ruleRenamed rs)
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
