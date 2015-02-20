{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           Control.Arrow          ((***))
import           Control.Lens           hiding (transform)
import           Control.Monad.Except
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as Map
import           Data.Monoid
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as LText
import qualified Data.Text.Lazy.Builder as Build
import           Data.Text.Manipulate
import           Gen.Model              hiding (Name)
import           Gen.OrdMap             (OrdMap)
import qualified Gen.OrdMap             as OrdMap
import           Gen.Types
import qualified HIndent
import           Language.Haskell.Exts  hiding (name)
import           Prelude                hiding (Enum)

data Uniq = Uniq
    { field  :: Text -> Name
    , branch :: Text -> Name
    }

unique :: Text -> Uniq
unique k = Uniq (f ('_' `Text.cons` Text.toLower k)) (f (Text.toUpper k))
  where
    f x = Ident . Text.unpack . mappend x . upperHead

data TypeOf
    = TCore Type
    | TLib  Type
      deriving (Eq, Show)

-- TODO:
-- render type class instances
-- render lenses per field

-- Just focus on getting any shape passed rendered, the filtering, selecting,
-- and massaging of type names should happen in the Override module on the
-- shapes hashmap.

-- Left: signifies a Haskell primitive, Right: a preserved library type.
typeOf :: Text -> Shape -> TypeOf
typeOf n = \case
    SStruct x -> TLib  (tyCon n)
    SList   x -> TCore (list x)
    SMap    x -> TCore (hmap x)
    SBlob   x -> TCore (stream x)
    SBool   x -> TCore (tyCon "Bool")
    -- FIXME: This is dependent on the service.
    STime   x -> TCore (time x)
    SDouble _ -> TCore (tyCon "Double")
    SInt    x -> TCore (natural x "Int")
    SLong   _ -> TCore (natural x "Integer")
  where
    list = undefined -- (List e a) || (List1 e a)

    hmap = undefined -- (Map k v) || (EMap e i j k v)

    stream = undefined -- figure out streaming or not

    time = tyCon
         . Text.pack
         . show
         . fromMaybe RFC822
         . view timeTimestampFormat

    natural x
        | x ^. numMin > Just 0 = const (tyCon "Natural")
        | otherwise            = tyCon

transform :: Text -> Prefix Shape -> Maybe Decl
transform t (Prefix p s) =
    case s of
        SStruct x -> Just $ recDecl n k (x ^. structMembers) d
        SEnum   x -> Just $ sumDecl n k (x ^. enumValues) d
--        SString x -> Just $ recDecl n k (OrdMap.fromList [(Member t t, refer "Text")]) d
    -- SBlob   x -> f x
    -- SBool   x -> f x
    -- STime   x -> f x
    -- SInt    x -> f x
    -- SDouble x -> f x
    -- SLong   x -> f x
        _         -> Nothing
  where
    k = unique p
    n = name t
    d = derive [Ident "Eq", Ident "Show"]

sumDecl :: Name -> Uniq -> HashMap Text Text -> [Deriving] -> Decl
sumDecl n k vs = dataDecl n (sumCtor k `map` Map.keys vs)

recDecl :: Name -> Uniq -> OrdMap Member Ref -> [Deriving] -> Decl
recDecl n k ms = dataDecl n [recCtor n (fields k ms)]

dataDecl :: Name -> [QualConDecl] -> [Deriving] -> Decl
dataDecl n cs = DataDecl empty (dataOrNew cs) [] n [] cs

sumCtor :: Uniq -> Text -> QualConDecl
sumCtor k b = ctor (ConDecl (branch k b) [])

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

fields :: Uniq -> OrdMap Member Ref -> [([Name], Type)]
fields k = map (f *** g) . OrdMap.toList
  where
    f :: Member -> [Name]
    f = (:[]) . field k . _memName

    g :: Ref -> Type
    g = TyCon . UnQual . Ident . Text.unpack . _refShape

derive :: [Name] -> [Deriving]
derive = map ((,[]) . UnQual)

name :: Text -> Name
name = Ident . Text.unpack

srcLoc :: Name -> SrcLoc
srcLoc = \case
    Ident  n -> SrcLoc n 0 0
    Symbol n -> SrcLoc n 0 0

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
