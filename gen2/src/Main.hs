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
import           Control.Lens              hiding ((<.>), (</>))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.Either
import qualified Data.Foldable             as Fold
import qualified Data.HashMap.Strict       as Map
import           Data.Jason                (eitherDecode)
import           Data.List                 (sortBy)
import           Data.Monoid
import qualified Data.SemVer               as SemVer
import           Data.String
import qualified Data.Text                 as Text
import qualified Data.Text.IO              as Text
import qualified Data.Text.Lazy.Builder    as Build
import qualified Data.Text.Lazy.IO         as LText
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS hiding (encode)
import           Gen.IO
import qualified Gen.JSON                  as JSON
import qualified Gen.Library               as Library
import           Gen.Model
import qualified Gen.Override              as Override
import           Gen.Types
import           Options.Applicative
import           Prelude                   hiding (FilePath)
import           System.Directory.Tree
import           System.IO                 hiding (FilePath)

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
    -- Makes errors easier to read due to output location correspondence.
    scriptIO $ hSetBuffering stdout LineBuffering

    o <- scriptIO $ customExecParser (prefs showHelpOnError) options
        >>= validate

    forM_ (o ^. optModels) $ \d -> do
        s <- service d (o ^. optOverrides)
        d <- writeTree $ Library.tree (o ^. optOutput) undefined s --(o ^. optTemplates)
        return ()

-- modules:
-- Gen.AST.hs     -> transform the model into a Haskell AST
-- Gen.Library.hs -> directory structure, assets, rendering of .hs files

--        scriptIO . LText.writeFile "test.hs" . Build.toLazyText . Fold.foldMap (<> "\n") $ rights xs

                -- Left  e -> scriptIO (putStrLn e)
                -- Right x -> LText.putStrLn (Build.toLazyText x)

--        scriptIO $ either putStrLn Text.putStrLn (pretty $ typesModule s)
--        say "Completed" (s ^. svcName)

--    say "Completed" (Text.pack $ show (length (o ^. optModels)) ++ " models.")

version :: FilePath -> Script FilePath
version d = do
    fs <- sortBy (flip compare) <$> scriptIO (FS.listDirectory d)
    f  <- tryHead ("Failed to get model version from " ++ show d) fs
    return (basename f)

service :: FilePath -> FilePath -> Script (Service (Prefix Shape))
service d o = do
    say "Load Service" (encode d)
    v <- version d
    x <- decode override
    y <- JSON.merge <$> sequence
        [ pure x
        , decode (normal v)
        , JSON.def "waiters"    $ decode (waiters v)
        , JSON.def "pagination" $ decode (pagers  v)
        ]
    either (throwError . msg) Override.service (JSON.parse y)
  where
    normal  = path "normal.json"
    waiters = path "waiters.json"
    pagers  = path "paginators.json"

    override = o </> basename d <.> "json"
    path e v = d </> v <.> e

    decode = fileContents >=> hoistEither . eitherDecode

    msg = mappend ("Error in " ++ show d ++ ": ")

 -- * Calculate a stable 'amazonka-*' library name per service
 -- * Ensure every shape has documentation (even if 'documentation is missing' string)
 -- * Ensure every ref for a shape has documentation, same as above
 -- * Ensure timestamp format for metadata is set
 -- * Parse/create the Endpoints model
 -- * Use a post-processing step (similar to stylish-haskell) which wraps all comment lines to 80 chars

-- * Tag the different 'Doc' usages with their parent type and use Data.Default instances
--   to select the default documentation for that type using 'fromMaybe def'.
