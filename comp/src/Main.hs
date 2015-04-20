{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
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

import           Compiler.IO
import           Compiler.JSON
import           Compiler.Model
import           Compiler.Types
import           Control.Error
import           Control.Lens               hiding ((<.>), (??))
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State
import           Control.Monad.Trans.Except
import           Data.Either
import           Data.Jason
import           Data.List                  (nub, sortBy)
import           Data.Monoid
import qualified Data.SemVer                as SemVer
import           Data.String
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Filesystem                 as FS
import           Filesystem.Path.CurrentOS
import           Formatting
import           Formatting.Time
import           Options.Applicative        hiding (optional)
import qualified Text.EDE                   as EDE

data Opt = Opt
    { _optOutput    :: Path
    , _optModels    :: [Path]
    , _optOverrides :: Path
    , _optTemplates :: Path
    , _optAssets    :: Path
    , _optRetry     :: Path
    , _optVersion   :: SemVer.Version
    } deriving (Show)

makeLenses ''Opt

parser :: Parser Opt
parser = Opt
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
  where
    string :: IsString a => ReadM a
    string = eitherReader (Right . fromString)

options :: ParserInfo Opt
options = info (helper <*> parser) fullDesc

validate :: MonadIO m => Opt -> m Opt
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
    check :: (MonadIO m, MonadState s m) => Lens' s Path -> m ()
    check l = gets (view l) >>= canon >>= assign l

    canon :: MonadIO m => Path -> m Path
    canon = liftIO . FS.canonicalizePath

main :: IO ()
main = do
    Opt{..} <- customExecParser (prefs showHelpOnError) options
        >>= validate

    run $ do
        forM_ _optModels $ \f -> do
            say ("Retrieving model versions from " % path) f

            m@Model{..} <- modelFromDir =<< listDirectory f
            let Ver{..} = m ^. latest

            say ("Using " % stext % " version " % dateDash % ", out-of-date " % dateDashes)
                _modName
                _verDate
                (m ^.. unused . verDate)

            api <- parseObject . mergeObjects =<< sequence
                [ required (_optOverrides </> (m ^. override))
                , required _verNormal
                , optional _verWaiters
                , optional _verPagers
                ]

            obj <- return (api :: Object)

            return ()

        say ("Successfully processed " % int % " models.")
            (length _optModels)

required :: MonadIO m => Path -> EitherT LazyText m Object
required f = noteT (format ("Unable to find " % path) f) (readByteString f)
    >>= decodeObject

optional :: MonadIO m => Path -> EitherT LazyText m Object
optional f = runMaybeT (readByteString f)
    >>= maybe (return mempty) decodeObject
