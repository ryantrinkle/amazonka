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
import           Control.Lens              (Lens', assign, makeLenses, view,
                                            (^.))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.HashMap.Strict       as Map
import           Data.Jason                (eitherDecode)
import           Data.List                 (sort)
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.AST
import           Gen.IO
import           Gen.JSON
import           Gen.Model
import           Gen.Types
import           Options.Applicative
import           Prelude                   hiding (FilePath)

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
    canon = liftIO . FS.canonicalizePath

main :: IO ()
main = runScript $ do
--    hSetBuffering stdout LineBuffering

    o <- scriptIO $ customExecParser (prefs showHelpOnError) options
        >>= validate

    forM_ (o ^. optModels) $ \d -> do
        s <- service d (o ^. optOverrides)
        forM_ (Map.toList $ s ^. svcShapes) $ \(k, v) ->
            scriptIO $ either putStrLn Text.putStrLn (declare k v)

--        scriptIO $ either putStrLn Text.putStrLn (pretty $ typesModule s)
--        say "Completed" (s ^. svcName)

--    say "Completed" (Text.pack $ show (length (o ^. optModels)) ++ " models.")

service :: FilePath -> FilePath -> Script Service
service d o = do
--    say "Load Service" (encode d)
    v <- version
    x <- decode override
    y <- mergeObjects <$> sequence
        [ pure x
        , decode (normal v)
        , def "waiters"    $ decode (waiters v)
        , def "pagination" $ decode (pagers  v)
        ]
    case parse y of
        Left  e -> throwError $ "Error parsing " ++ show d ++ " with " ++ e
        Right x -> return x
  where
    version = do
        fs <- reverse . sort <$> scriptIO (FS.listDirectory d)
        f  <- tryHead ("Failed to get model version from " ++ show d) fs
        return (basename f)

    decode = contents >=> hoistEither . eitherDecode

    normal   = path "normal.json"
    waiters  = path "waiters.json"
    pagers   = path "paginators.json"

    path e v = d </> v <.> e
    override = o </> name <.> "json"

    name = basename d

 -- * Calculate a stable 'amazonka-*' library name per service
 -- * Ensure every shape has documentation (even if 'documentation is missing' string)
 -- * Ensure every ref for a shape has documentation, same as above
 -- * Ensure timestamp format for metadata is set
 -- * Parse/create the Endpoints model
 -- * Use a post-processing step (similar to stylish-haskell) which wraps all comment lines to 80 chars

-- * Tag the different 'Doc' usages with their parent type and use Data.Default instances
--   to select the default documentation for that type using 'fromMaybe def'.
