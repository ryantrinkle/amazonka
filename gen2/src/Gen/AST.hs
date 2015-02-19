{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           Control.Arrow                ((***))
import           Control.Lens                 hiding (transform)
import           Control.Monad.Except
import qualified Data.HashMap.Strict          as Map
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import qualified Data.Text.Lazy               as LText
import qualified Data.Text.Lazy.Builder       as Build
import           Data.Text.Manipulate
import           Gen.Model                    hiding (Name)
import           Gen.OrdMap                   (OrdMap)
import qualified Gen.OrdMap                   as OrdMap
import           Gen.Types
import qualified HIndent
import           Language.Haskell.Exts        hiding (name)
import           Language.Haskell.Exts.Pretty
import           Prelude                      hiding (Enum)

data Uniq = Uniq
    { field  :: Text -> Name
    , branch :: Text -> Name
    }

unique :: Text -> Uniq
unique k = Uniq (f ('_' `Text.cons` Text.toLower k)) (f (Text.toUpper k))
  where
    f x = Ident . Text.unpack . mappend x . upperHead

-- TODO:
-- render type class instances

-- Just focus on getting any shape passed rendered, the filtering, selecting,
-- and massaging of type names should happen in the Override module on the
-- shapes hashmap.

transform :: Text -> Prefix Shape -> Maybe Decl
transform (name -> n) (Prefix p s) =
    case s of
    SStruct x -> Just $ record  n (fields k (x ^. structMembers)) d
    SEnum   x -> Just $ nullary n k (x ^. enumValues) d

    -- SString x -> f x Render as newtype
    -- SBlob   x -> f x
    -- SBool   x -> f x
    -- STime   x -> f x
    -- SInt    x -> f x
    -- SDouble x -> f x
    -- SLong   x -> f x
    _         -> Nothing
  where
    k = unique p
    d = derive [Ident "Eq", Ident "Show"]

--nullary :: Name
nullary n k cs = DataDecl l DataType [] n [] (f `map` Map.toList cs)
  where
    f (b, _) = QualConDecl l [] [] (ConDecl (branch k b) [])

    l = location n

record :: Name -> [([Name], Type)] -> [Deriving] -> Decl
record n fs = DataDecl l s [] n [] [QualConDecl l [] [] (RecDecl n fs)]
  where
    s | [_] <- fs = NewType
      | otherwise = DataType

    l = location n

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

location :: Name -> SrcLoc
location = \case
    Ident  n -> SrcLoc n 0 0
    Symbol n -> SrcLoc n 0 0

pretty :: (Monad m, MonadError String m, Pretty a) => a -> m LText.Text
pretty d = -- hoist $ HIndent.reformat HIndent.johanTibell Nothing (LText.pack x)
    return (LText.pack x)
  where
    hoist (Left  e) = throwError (e ++ ": ->" ++ x)
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
