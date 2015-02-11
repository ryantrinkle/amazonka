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
import qualified Control.Foldl             as Fold
import           Control.Lens              (Lens', assign, makeLenses, view,
                                            (^.))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.String
import qualified Data.Text                 as Text
import           Filesystem.Path.CurrentOS hiding (encode)
import           Khan.Gen.IO
import           Khan.Gen.JSON
import           Khan.Gen.Model
import           Khan.Gen.Types
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import qualified Turtle
import           Turtle.Prelude
import           Turtle.Shell              hiding (view)

data Options = Options
    { _optOutput    :: FilePath
    , _optModels    :: [FilePath]
    , _optOverrides :: FilePath
    , _optTemplates :: FilePath
    , _optAssets    :: FilePath
    , _optRetry     :: FilePath
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

validate :: MonadIO m => Options -> m Options
validate o = flip execStateT o $ do
    sequence_
        [ check optOutput
        , check optOverrides
        , check optTemplates
        , check optAssets
        , check optRetry
        ]
    mapM canon (o ^. optModels) >>= assign optModels
  where
    check :: (MonadIO m, MonadState s m) => Lens' s FilePath -> m ()
    check l = gets (view l) >>= canon >>= assign l

    canon :: MonadIO m => FilePath -> m FilePath
    canon = liftIO . realpath

main :: IO ()
main = sh $ do
    o <- liftIO (customExecParser (prefs showHelpOnError) options)
        >>= validate

    forM_ (o ^. optModels) $ \d -> do
        s <- service d (o ^. optOverrides)
        say "Completed" (s ^. metaServiceFullName)

    say "Completed" (Text.pack $ show (length (o ^. optModels)) ++ " models.")

service :: FilePath -> FilePath -> Shell Service
service d o = do
    say "Load Service" (encode d)
    v <- version
    x <- requireObject override
    y <- mergeObjects <$> sequence
        [ pure x
        , requireObject (normal v)
        , optionalObject "waiters"    (waiters v)
        , optionalObject "pagination" (pagers  v)
        ]
    case parseObject y of
        Left  e -> failure d e
        Right f -> return $ f (encode name)
  where
    version = liftIO $ do
        m <- fold (ls d) Fold.head
        maybe (failure d "Unable to find versioned model")
              (pure . fromText . Text.takeWhile (/= '.') . encode)
              m

    normal   = path "normal.json"
    waiters  = path "waiters.json"
    pagers   = path "paginators.json"

    path e v = d </> v <.> e
    override = o </> name <.> "json"

    name = basename d
