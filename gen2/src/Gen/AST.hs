{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
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

module Gen.AST
     ( ToEnv (..)
     , Library
     , HasLibrary (..)
     , Cabal
     , cabal
     , tyCon
     ) where

import           Control.Applicative    (Applicative, pure, (<$>))
import           Control.Arrow          ((***))
import           Control.Error
import           Control.Lens           (makeClassy, makeLenses, view, (^.))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types       (Pair)
import           Data.Bifunctor
import           Data.Foldable          (foldr')
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.List              (sort)
import           Data.Maybe
import           Data.Monoid            hiding (Sum)
import qualified Data.SemVer            as SemVer
import           Data.String
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as Build
import           Data.Text.Manipulate
import           Data.Traversable       (traverse)
import           Gen.Documentation      as Doc
import           Gen.JSON
import           Gen.Model              hiding (Name)
import           Gen.OrdMap             (OrdMap)
import qualified Gen.OrdMap             as OrdMap
import           Gen.Types
import qualified HIndent
import           Language.Haskell.Exts  hiding (extensions, loc, name)
import           Prelude                hiding (Enum)

pretty :: (Monad m, MonadError String m, Pretty a) => a -> m LText.Text
pretty d = hoist $ HIndent.reformat HIndent.johanTibell Nothing (LText.pack x)
  where
    hoist (Left  e) = throwError (e ++ "\nDecl: " ++ x)
    hoist (Right o) = return (Build.toLazyText o)

    x = prettyPrintStyleMode style' mode' d

    style' = style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }

    mode' = defaultMode
        { spacing = False
        , layout  = PPNoLayout
        }

class ToEnv a where
    toEnv :: (Applicative m, MonadError String m) => a -> m Value

    default toEnv :: (Applicative m, MonadError String m, ToJSON a)
                  => a
                  -> m Value
    toEnv = pure . toJSON

(.-) :: (Applicative m, Monad m, MonadError String m, ToEnv a)
     => Text
     -> a
     -> m Pair
k .- v = (k,) <$> toEnv v

env :: (Monad m, MonadError String m) => [m Pair] -> m Value
env = liftM object . sequence

instance ToEnv Text
instance ToEnv LText.Text
instance ToEnv (Above Doc)
instance ToEnv (Below Doc)
instance ToEnv (Blank Doc)

instance ToEnv a => ToEnv [a] where
    toEnv = fmap toJSON . traverse toEnv

instance ToEnv v => ToEnv (TextMap v) where
    toEnv = fmap toJSON . traverse toEnv

instance ToEnv ModuleName   where toEnv = pure . toJSON . prettyPrint
instance ToEnv ModulePragma where toEnv = pure . toJSON . prettyPrint
instance ToEnv ImportDecl   where toEnv = pure . toJSON . prettyPrint
instance ToEnv Name         where toEnv = pure . toJSON . prettyPrint
instance ToEnv Decl         where toEnv = fmap toJSON . pretty

data Fun = Fun Name Doc Decl Decl

instance ToEnv Fun where
    toEnv (Fun n doc sig decl) = env
        [ "name"        .- n
        , "comment"     .- Above 0 doc
        , "signature"   .- sig
        , "declaration" .- decl
        ]

data Data
    = Prod (Typed Struct) Doc Decl Fun (TextMap Fun) [Decl]
    | Sum Enum Doc Decl [Decl]

-- Sorting of types?

instance ToEnv Data where
    toEnv = env . \case
        Prod _ doc decl ctor ls is ->
            [ "type"        .- Text.pack "product"
            , "constructor" .- ctor
            , "comment"     .- Above 0 doc
            , "declaration" .- decl
            , "lenses"      .- ls
            , "instances"   .- is
            ]
        Sum  _ doc decl is ->
            [ "type"        .- Text.pack "sum"
            , "comment"     .- Above 0 doc
            , "declaration" .- decl
            , "instances"   .- is
            ]

data Mod = Mod ModuleName [ModulePragma] [ModulePragma] [ImportDecl] (TextMap Data)

instance ToEnv Mod where
    toEnv (Mod n es os is ds) = env
        [ "name"       .- n
        , "extensions" .- es
        , "options"    .- os
        , "imports"    .- is
        , "shapes"     .- ds
        ]

data Library = Library
    { _libService    :: Mod
    , _libTypes      :: Mod
    , _libOperations :: [Mod]
    , _libWaiters    :: Mod
    }

-- instance ToEnv Library where
--     toEnv Library{..} = env
--         [ -- "service"    .- _libService
-- --          "types"      .- _libTypes
--         -- , "operations" .- _libOperations
--         -- , "waiters"    .- _libWaiters
--         ]

makeClassy ''Library

data Cabal = Cabal
    { _cblVersion :: SemVer.Version
    , _cblService :: Service (Typed Shape) (Untyped Ref)
    , _cblLibrary :: Library
    }

makeLenses ''Cabal

instance HasMetadata Cabal where
    metadata = cblService . svcMetadata

instance HasService Cabal (Typed Shape) (Untyped Ref) where
    service = cblService

instance HasLibrary Cabal where
    library = cblLibrary

instance ToEnv Cabal where
    toEnv c = env
       [ "name"             .- view svcName c
       , "library"          .- view svcLibrary c
       , "version"          .- SemVer.toText (c ^. cblVersion)
       , "documentation"    .- Blank 4 (c ^. svcDocumentation)
       , "documentationUrl" .- view svcDocumentationUrl c
--       , "modules"          .- view cblModules c
       ]

cabal :: SemVer.Version -> Service (Typed Shape) (Untyped Ref) -> Cabal
cabal v s = Cabal v s (Library undefined (typesMod n s) [] undefined)
  where
    n = s ^. svcAbbrev

serviceMod :: Mod
serviceMod = undefined

typesMod :: HasService s (Typed Shape) b => Text -> s -> Mod
typesMod n s = Mod (moduleName n) es os is ts
  where
    es = extensions
       [ "DataKinds"
       , "DeriveGeneric"
       , "FlexibleInstances"
       , "GeneralizedNewtypeDeriving"
       , "LambdaCase"
       , "NoImplicitPrelude"
       , "OverloadedStrings"
       , "RecordWildCards"
       , "TypeFamilies"
       , "ViewPatterns"
       ]

    os = options
       [ "-fno-warn-unused-imports"
       ]

    is = imports
       [ ("Network.AWS.Prelude", False)
       , ("Network.AWS.Signing", False)
       , ("GHC.Exts",            True)
       ]

    ts = Map.fromList . mapMaybe f $ Map.toList (s ^. svcShapes)
      where
        f (n, s) = (n,) <$> shapeDecl n s

operationMod :: Mod
operationMod = undefined

waitersMod :: Mod
waitersMod = undefined

extensions :: [Text] -> [ModulePragma]
extensions = map (LanguagePragma loc . (:[]) . name) . sort

options :: [Text] -> [ModulePragma]
options = map (OptionsPragma loc (Just GHC) . Text.unpack . (`Text.snoc` ' '))

imports :: [(Text, Bool)] -> [ImportDecl]
imports = map f
  where
    f (n, q) = ImportDecl loc (moduleName n) q False False Nothing Nothing Nothing

shapeDecl :: Text -> Typed Shape -> Maybe Data
shapeDecl t s = case s of
    SStruct x -> Just $ Prod x doc (recDecl n (x ^. structMembers) d) (structCtor t x) mempty []
    SEnum   x -> Just $ Sum  x doc (sumDecl n (x ^. enumValues)    d) []
    _         -> Nothing
  where
    n = name t
    d = derive [Ident "Eq", Ident "Show"]

    doc = case s of
        SStruct {} -> f "Undocumented type."
        SEnum   {} -> f "Undocumented enumeration type."
        _          -> f "Undocumented type."
      where
        f = flip fromMaybe (s ^. documentation)

data Field = Field
    { _fldParam  :: Name
    , _fldName   :: Member
    , _fldType   :: Type
    , _fldUpdate :: FieldUpdate
    }

structCtor :: Text -> Typed Struct -> Fun
structCtor t (structFields -> fs) = Fun c d sig fun
  where
    d = fromString ("'" <> Text.unpack t <> "' smart constructor.")

    c = name (lowerHead t)
    n = name t

    fun = sfun loc c ps (UnGuardedRhs (RecConstr (UnQual n) us)) (BDecls [])
    sig = typeSig c (TyCon (UnQual n)) ts

    ps = map _fldParam  fs
    ts = map _fldType   fs
    us = map _fldUpdate fs

structFields :: Typed Struct -> [Field]
structFields = zipWith mk [1..] . OrdMap.toList . view structMembers
  where
    mk :: Int -> (Member, Typed Ref) -> Field
    mk n (k, v) =
        let p = Ident ("p" ++ show n)
         in Field
            { _fldParam  = p
            , _fldName   = k
            , _fldType   = _refShape v
            , _fldUpdate = FieldUpdate (UnQual (field k)) (Var (UnQual p))
            }

typeSig :: Name -> Type -> [Type] -> Decl
typeSig n t = TypeSig loc [n] . foldr' (\x g -> TyFun x g) t

sumDecl :: Name -> OrdMap Member Text -> [Deriving] -> Decl
sumDecl n vs = dataDecl n (sumCtor `map` OrdMap.keys vs)

recDecl :: Name -> OrdMap Member (Ref Type) -> [Deriving] -> Decl
recDecl n ms = dataDecl n [recCtor n (fields ms)]

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl loc (dataOrNew cs) [] n [] cs

sumCtor :: Member -> QualConDecl
sumCtor m = ctor (ConDecl (branch m) [])
  where
    branch :: Member -> Name
    branch (Member p _ n) = prefixed (Text.toUpper <$> p) n

recCtor :: Name -> [([Name], Type)] -> QualConDecl
recCtor n = ctor . RecDecl n

ctor :: ConDecl -> QualConDecl
ctor = QualConDecl loc [] []

tyCon :: Text -> Type
tyCon = TyCon . UnQual . name

dataOrNew :: [QualConDecl] -> DataOrNew
dataOrNew = \case
    [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
    _                                   -> DataType

fields :: OrdMap Member (Ref Type) -> [([Name], Type)]
fields = map (((:[]) . field) *** _refShape) . OrdMap.toList
  where

derive :: [Name] -> [Deriving]
derive = map ((,[]) . UnQual)

prefixed :: Maybe Text -> Text -> Name
prefixed (Just x) = name . mappend x . upperHead
prefixed Nothing  = name . upperHead

field :: Member -> Name
field (Member p _ n) = prefixed (Text.cons '_' . Text.toLower <$> p) n

name :: Text -> Name
name = Ident . Text.unpack

moduleName :: Text -> ModuleName
moduleName = ModuleName . Text.unpack

loc :: SrcLoc
loc = SrcLoc "" 0 0

-- pretty :: (Monad m, MonadError String m) => Typed Shape -> m LText.Text
-- pretty = prettyP

-- transform :: Text -> Prefix Shape -> Maybe Decl
-- transform (Text.unpack -> name) p =
--     case p ^. prefItem of
--         SStruct x -> Just (struct x)
--         _         -> Nothing
--   where
--     struct :: Struct -> Decl
--     struct Struct{..} = record (fields _structMembers) (derive ["Eq", "Show"])

--     record :: [([Name], Type)] -> [Deriving] -> Decl
--     record fs = DataDecl l s [] n [] ctor
--       where
--         ctor = [QualConDecl (SrcLoc name 0 10) [] [] (RecDecl n fs)]

--         s | [_] <- fs = NewType
--           | otherwise = DataType

--     fields :: OrdMap Member Ref -> [([Name], Type)]
--     fields = map f . OrdMap.toList
--       where
--         f (Text.unpack . _memName -> k, Text.unpack . _refShape -> v) =
--             ([Ident k], TyCon (UnQual (Ident v)))

--     l = SrcLoc name 0 0
--     n = Ident name

-- class AST a where
--     transform :: Text -> a -> Either String Decl

-- instance AST (Prefix Shape) where
--     transform n p =
--         case p ^. prefItem of
--             -- SList   x -> f x
--             -- SMap    x -> f x
--             SStruct x -> f x
--             -- SString x -> f x
--             -- SEnum   x -> f x
--             -- SBlob   x -> f x
--             -- SBool   x -> f x
--             -- STime   x -> f x
--             -- SInt    x -> f x
--             -- SDouble x -> f x
--             -- SLong   x -> f x
--           where
--             f x = transform n (p & prefItem .~ x)

-- instance AST (Prefix Struct) where
--     transform n p =


-- instance AST (Prefix List) where
-- instance AST (Prefix Map) where
-- instance AST (Prefix Chars) where
-- instance AST (Prefix Enum) where
-- instance AST (Prefix Blob) where
-- instance AST (Prefix Boolean) where
-- instance AST (Prefix Time) where
-- instance AST (Prefix (Number Int)) where
-- instance AST (Prefix (Number Double)) where
-- instance AST (Prefix (Number Integer)) where
