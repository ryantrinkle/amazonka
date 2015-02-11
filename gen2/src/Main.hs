{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Control.Applicative
import           Control.Error
import           Control.Lens           (Lens', assign, makeLenses, view, (^.))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Monoid
import qualified Data.SemVer            as SemVer
import           Data.String
import qualified Data.Text              as Text
import           Khan.Gen.IO
import           Khan.Gen.Model
import           Khan.Gen.Types
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO

data Options = Options
    { _optOutput    :: OutputDir
    , _optModels    :: [ModelPath]
    , _optOverrides :: OverrideDir
    , _optTemplates :: TemplateDir
    , _optAssets    :: AssetDir
    , _optRetry     :: RetryPath
    , _optVersion   :: SemVer.Version
    } deriving (Show)

makeLenses ''Options

options :: ParserInfo Options
options = info (helper <*> parser) fullDesc

parser :: Parser Options
parser = Options
    <$> option string
         ( long "out"
        <> metavar "DIR"
        <> help "Directory to place the generated library. [required]"
         )

    <*> some (option string
         ( long "model"
        <> metavar "PATH"
        <> help "Directory for a service's botocore models. [required]"
         ))

    <*> option string
         ( long "overrides"
        <> metavar "DIR"
        <> help "Directory containing amazonka overrides. [required]"
         )

    <*> option string
         ( long "templates"
        <> metavar "DIR"
        <> help "Directory containing ED-E templates. [required]"
         )

    <*> option string
         ( long "assets"
        <> metavar "PATH"
        <> help "Directory containing assets for generated libraries. [required]"
         )

    <*> option string
         ( long "retry"
        <> metavar "PATH"
        <> help "Path to the file containing retry definitions. [required]"
         )

    <*> option (eitherReader (SemVer.fromText . Text.pack))
         ( long "version"
        <> metavar "VER"
        <> help "Version of the library to generate. [required]"
         )

string :: IsString a => ReadM a
string = eitherReader (Right . fromString)

-- validate :: MonadIO m => Options -> m Options
-- validate o = flip execStateT o $ do
--     sequence_
--         [ check output
--         , check overrides
--         , check templates
--         , check assets
--         , check retry
--         ]
--     mapM canon (o ^. models)
--         >>= assign models

-- check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
-- check l = gets (view l) >>= canon >>= assign l

-- canon :: MonadIO m => FilePath -> m FilePath
-- canon = liftIO . canonicalizePath

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    o <- customExecParser (prefs showHelpOnError) options -- >>= validate
    print o
