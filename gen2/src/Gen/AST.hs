{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

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

import           Control.Lens
import           Data.Bifunctor
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Gen.Model
import           Gen.Types
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Exts.Pretty    (prettyPrint)
import qualified Language.Haskell.Stylish        as Style

pretty :: Pretty a => a -> Either String Text
pretty = second (Text.unlines . map Text.pack)
    . Style.runSteps [] Nothing steps
    . lines
    . prettyPrint
  where
    steps =
        [ Style.languagePragmas 80 Style.Vertical True
        , Style.imports 80 Style.Global
        , Style.records
        ]

data Meta = Meta Text Text
    deriving (Eq, Show)

instance SrcInfo Meta where
    toSrcInfo _ _ _ = Meta mempty mempty
    fromSrcInfo     = const (Meta mempty mempty)
    fileName        = const "gen"
    startLine       = const 0
    startColumn     = const 0

-- data DLens = DLens (Decl Meta) (Decl Meta)
-- data DType = DType (Decl Meta)
-- data DInst = DInst (Decl Meta)

declare :: Text -> Shape -> Either String Text
declare (Text.unpack -> name) = \case
    -- SList   x ->
    -- SMap    x ->
    SStruct x -> Right . Text.pack . prettyPrint $ struct x
    -- SString x ->
    -- SEnum   x ->
    -- SBlob   x ->
    -- SBool   x ->
    -- STime   x ->
    -- SInt    x ->
    -- SDouble x ->
    -- SLong   x ->

    _ -> Left ""
  where
    struct :: Struct -> Decl Meta
    struct Struct{..} = record (fields _structMembers) (derive ["Eq", "Show"])

    record :: [FieldDecl Meta] -> Maybe (Deriving Meta) -> Decl Meta
    record fs = DataDecl m s Nothing (DHead m n) ctor
      where
        ctor = [QualConDecl m Nothing Nothing (RecDecl m n fs)]

        s | [_] <- fs = NewType  m
          | otherwise = DataType m

    fields :: OrdMap Ref -> [FieldDecl Meta]
    fields (ordMap -> fs) = map f fs
      where
        f (Text.unpack -> k, Text.unpack . _refShape -> v) =
            FieldDecl m [Ident m k] (TyCon m (UnQual m (Ident m v)))

    derive :: [Text] -> Maybe (Deriving Meta)
    derive [] = Nothing
    derive ns = Just . Deriving m $ map f ns
      where
        f (Text.unpack -> n) =
            IRule m Nothing Nothing (IHCon m (UnQual m (Ident m n)))

    m = Meta mempty mempty
    n = Ident m name

-- lens = [sig, pat]
--   where
--     sig = TypeSig l [Ident l "naeRuleAction"] (TyApp l (TyApp l (TyCon l (UnQual l (Ident l "Lens'"))) (TyCon l (UnQual l (Ident l "NetworkAclEntry")))) (TyParen l (TyApp l (TyCon l (UnQual l (Ident l "Maybe"))) (TyCon l (UnQual l (Ident l "RuleAction"))))))
--     pat = PatBind l (PVar l (Ident l "naeRuleAction")) (UnGuardedRhs l (App l (App l (Var l (UnQual l (Ident l "lens"))) (Var l (UnQual l (Ident l "_naeRuleAction")))) (Paren l (Lambda l [PVar l (Ident l "s"),PVar l (Ident l "a")] (RecUpdate l (Var l (UnQual l (Ident l "s"))) [FieldUpdate l (UnQual l (Ident l "_naeRuleAction")) (Var l (UnQual l (Ident l "a")))]))))) Nothing

--     l = Meta mempty mempty


-- datatype = DataDecl l (DataType l) Nothing (DHead l name) [QualConDecl l Nothing Nothing record] (Just derive)
--   where
--     name = Ident l "Foo"

--     derive = Deriving l
--         [ IRule l Nothing Nothing (IHCon l (UnQual l (Ident l "Eq")))
--         , IRule l Nothing Nothing (IHCon l (UnQual l (Ident l "Show")))
--         ]

--     record = RecDecl l ctor fields

--     ctor = Ident l "Foo"

--     fields = [strict]

--     strict = FieldDecl l [Ident l "_barField"] (TyBang l (BangedTy l) (TyCon l (UnQual l (Ident l "Int"))))

--     l = Meta mempty mempty
