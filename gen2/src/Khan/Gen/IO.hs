{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Khan.Gen.IO
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.IO where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO.Class
import qualified Data.Aeson                as A
import           Data.Aeson.Encode.Pretty
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Jason                (eitherDecode')
import           Data.Jason.Types          (FromJSON, Object, Value (..),
                                            mkObject)
import           Data.List                 (sort)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding (encode)
import           Khan.Gen.JSON
import           Khan.Gen.Model
import           Khan.Gen.Types
import           Prelude                   hiding (FilePath)
import           Text.EDE                  (Template)
import qualified Text.EDE                  as EDE
import           Turtle.Prelude
import           Turtle.Shell              (Shell)

-- loadRetries :: RetryPath -> Script Retrier
-- loadRetries = requireObject >=> parse

-- Change namespace from Khan to just Gen again

requireObject :: FromJSON a => FilePath -> Shell a
requireObject p = loadObject p >>= either (failure p) return

optionalObject :: Text -> FilePath -> Shell Object
optionalObject k p = either (const obj) id <$> loadObject p
  where
    obj = mkObject [(k, Object mempty)]

loadObject :: FromJSON a => FilePath -> Shell (Either String a)
loadObject p = liftIO $ do
    say "Load Object" (encode p)
    b <- testfile p
    if b
        then eitherDecode' . LBS.fromStrict <$> FS.readFile p
        else return . Left $ "Unable to find " ++ show p

failure :: MonadIO m => FilePath -> String -> m a
failure !p !msg = liftIO . die $ Text.pack msg <> " in " <> encode p

say :: MonadIO m => Text -> Text -> m ()
say !x !msg = liftIO . Text.putStrLn $ "[ " <> y <> "] " <> msg
  where
    y | n > 0     = x <> Text.replicate n " "
      | otherwise = x

    n = 17 - Text.length x

-- createDir :: MonadIO m => FilePath -> EitherT String m ()
-- createDir = scriptIO . createDirectoryIfMissing True

-- copyContents :: FilePath -> FilePath -> Script ()
-- copyContents s d = do
--     fs <- map (combine s) . filter dots <$> scriptIO (getDirectoryContents s)
--     scriptIO (mapM_ copy fs)
--   where
--     copy f@(dest -> p) = say "Copy" p >> copyFile f p

--     dest f = d </> takeFileName f

-- renderFile :: ToFilePath a
--            => Text
--            -> Template
--            -> FilePath
--            -> a
--            -> A.Object
--            -> Script ()
-- renderFile lbl t d p env = do
--     createDir (dropFileName f)
--     say lbl f
--     txt <- hoistEither (EDE.eitherRenderWith genFilters t env)
--     scriptIO (LText.writeFile f txt)
--   where
--     f = d </> toFilePath p

-- writeJSON :: A.ToJSON a => FilePath -> a -> Script ()
-- writeJSON p x = do
--     say "Write JSON" p
--     scriptIO (LBS.writeFile p (encodePretty x))

-- say :: MonadIO m => Text -> String -> m ()
-- say x msg = liftIO . Text.putStrLn $ "[ " <> y <> "] " <> Text.pack msg
--   where
--     y | n > 0     = x <> Text.replicate n " "
--       | otherwise = x

--     n = 17 - Text.length x

-- dots :: FilePath -> Bool
-- dots "."  = False
-- dots ".." = False
-- dots _    = True