{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

import           Control.Applicative    (Applicative, pure, (<$>))
import           Control.Arrow          ((***))
import           Control.Error
import           Control.Lens           ((^.))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Types       (Pair)
import           Data.Bifunctor
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.SemVer            as SemVer
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as Build
import           Data.Text.Manipulate
import           Data.Traversable       (traverse)
import           Gen.Documentation
import           Gen.JSON
import           Gen.Model              hiding (Name)
import           Gen.OrdMap             (OrdMap)
import qualified Gen.OrdMap             as OrdMap
import           Gen.Types
import qualified HIndent
import           Language.Haskell.Exts  hiding (name)
import           Prelude                hiding (Enum)

class ToEnv a where
    toEnv :: (Applicative m, MonadError String m) => a -> m Value

    default toEnv :: (Applicative m, MonadError String m, Pretty a)
                  => a
                  -> m Value
    toEnv = fmap toJSON . pretty

(.-) :: (Applicative m, Monad m, MonadError String m, ToEnv a)
     => Text
     -> a
     -> m Pair
k .- v = (k,) <$> toEnv v

env :: (Monad m, MonadError String m) => [m Pair] -> m Value
env = liftM object . sequence

instance ToEnv Text       where toEnv = pure . toJSON
instance ToEnv LText.Text where toEnv = pure . toJSON
instance ToEnv Doc        where toEnv = pure . toJSON

instance ToEnv a => ToEnv [a] where
    toEnv = fmap toJSON . traverse toEnv

instance ToEnv v => ToEnv (TextMap v) where
    toEnv = fmap toJSON . traverse toEnv

instance ToEnv ModuleName
instance ToEnv ModulePragma
instance ToEnv ImportDecl
instance ToEnv Decl

data Com = Com Doc Decl

instance ToEnv Com where
    toEnv (Com x y) = env
        [ "comment"     .- x
        , "declaration" .- y
        ]

data Data = Data
    { _dataType   :: Typed Shape
    , _dataDecl   :: Decl
    , _dataCtor   :: Com
    , _dataLenses :: TextMap Com
    , _dataInst   :: [Decl]
    }

instance ToEnv Data where
    toEnv Data{..} = env
        [ "type"        .- Text.pack "<shape>"
        , "declaration" .- _dataDecl
        , "constructor" .- _dataCtor
        , "lenses"      .- _dataLenses
        , "instances"   .- _dataInst
        ]

data Mod = Mod
    { _modName      :: ModuleName
    , _modPragmas   :: [ModulePragma]
    , _modImports   :: [ImportDecl]
    , _modDataTypes :: TextMap Data
    }

instance ToEnv Mod where
    toEnv Mod{..} = env
        [ "name"      .- _modName
        , "pragmas"   .- _modPragmas
        , "imports"   .- _modImports
        , "datatypes" .- _modDataTypes
        ]

data Lib = Lib
    { _libService    :: Mod
    , _libTypes      :: Mod
    , _libOperations :: [Mod]
    , _libWaiters    :: Mod
    }

instance ToEnv Lib where
    toEnv Lib{..} = env
        [ "service"    .- _libService
        , "types"      .- _libTypes
        , "operations" .- _libOperations
        , "waiters"    .- _libWaiters
        ]

data Cabal = Cabal
    { _cblName             :: Text
    , _cblLibrary          :: Text
    , _cblVersion          :: SemVer.Version
    , _cblDocumentation    :: Doc
    , _cblDocumentationUrl :: Text
    , _cblModules          :: Lib
    }

instance ToEnv Cabal where
    toEnv Cabal{..} = env
       [ "name"             .- _cblName
       , "library"          .- _cblLibrary
       , "version"          .- SemVer.toText _cblVersion
       , "documentation"    .- _cblDocumentation
       , "documentationUrl" .- _cblDocumentationUrl
       , "modules"          .- _cblModules
       ]

cabal :: Service (Typed Shape) b -> Cabal
cabal s = undefined

datatype :: Text -> Typed Shape -> Maybe Data
datatype t s = do
    _ <- go s
    return $! Data
        { _dataType   = s
        , _dataDecl   = FunBind []
        , _dataCtor   = Com (format mempty) (FunBind [])
        , _dataLenses = mempty
        , _dataInst   = []
        }
  where
    go = \case
        SStruct x -> Just $ recDecl n (x ^. structMembers) d
        SEnum   x -> Just $ sumDecl n (x ^. enumValues) d
--        SString x -> Just $ recDecl n k (OrdMap.fromList [(Member t t, refer "Text")]) d
    -- SBlob   x -> f x
    -- SBool   x -> f x
    -- STime   x -> f x
    -- SInt    x -> f x
    -- SDouble x -> f x
    -- SLong   x -> f x
        _         -> Nothing

    n = name t
    d = derive [Ident "Eq", Ident "Show"]

-- unsafePretty :: Pretty a => a -> LText.Text
-- unsafePretty x = either error id e
--   where
--     e = pretty x :: Either String LText.Text

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

sumDecl :: Name -> OrdMap Member Text -> [Deriving] -> Decl
sumDecl n vs = dataDecl n (sumCtor `map` OrdMap.keys vs)

recDecl :: Name -> OrdMap Member (Ref Type) -> [Deriving] -> Decl
recDecl n ms = dataDecl n [recCtor n (fields ms)]

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl empty (dataOrNew cs) [] n [] cs

sumCtor :: Member -> QualConDecl
sumCtor m = ctor (ConDecl (branch m) [])
  where
    branch :: Member -> Name
    branch (Member p _ n) = prefixed (Text.toUpper <$> p) n

recCtor :: Name -> [([Name], Type)] -> QualConDecl
recCtor n = ctor . RecDecl n

ctor :: ConDecl -> QualConDecl
ctor = QualConDecl empty [] []

tyCon :: Text -> Type
tyCon = TyCon . UnQual . name

empty :: SrcLoc
empty = SrcLoc "empty" 0 0

dataOrNew :: [QualConDecl] -> DataOrNew
dataOrNew = \case
    [QualConDecl _ _ _ (RecDecl _ [_])] -> NewType
    _                                   -> DataType

fields :: OrdMap Member (Ref Type) -> [([Name], Type)]
fields = map (((:[]) . field) *** _refShape) . OrdMap.toList
  where
    field :: Member -> Name
    field (Member p _ n) = prefixed (Text.cons '_' . Text.toLower <$> p) n

derive :: [Name] -> [Deriving]
derive = map ((,[]) . UnQual)

prefixed :: Maybe Text -> Text -> Name
prefixed (Just x) = name . mappend x . upperHead
prefixed Nothing  = name . upperHead

name :: Text -> Name
name = Ident . Text.unpack

srcLoc :: Name -> SrcLoc
srcLoc = \case
    Ident  n -> SrcLoc n 0 0
    Symbol n -> SrcLoc n 0 0

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
