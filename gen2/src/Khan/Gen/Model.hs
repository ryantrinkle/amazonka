{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Khan.Gen.Model
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Model
   ( module Khan.Gen.Model
   , module Model
   ) where

import           Control.Applicative
import           Control.Lens
import           Data.HashMap.Strict      (HashMap)
import           Data.Jason
import           Data.Jason.Types         (mkObject, unObject)
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Khan.Gen.Model.Index     as Model
import           Khan.Gen.Model.Paginator as Model
import           Khan.Gen.Model.Retrier   as Model
import           Khan.Gen.Model.URI       as Model
import           Khan.Gen.Model.Waiter    as Model
import           Khan.Gen.TH
import           Khan.Gen.Types

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq, Show)

deriveFromJSON upper ''Method

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show)

deriveFromJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show)

deriveFromJSON spinal ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

deriveFromJSON spinal ''Checksum

data Location
    = Headers
    | Header
    | Uri
    | Querystring
    | StatusCode
    | Body
      deriving (Eq, Show)

deriveFromJSON spinal ''Location

data XMLNS = XMLNS
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

makeLenses ''XMLNS
deriveFromJSON spinal ''XMLNS

-- | A reference to a 'Shape', plus any additional annotations
-- specific to the point at which the type is de/serialised.
data Ref = Ref
    { _refShape         :: !Text
    , _refDocumentation :: Maybe Text
    , _refLocation      :: Maybe Location
    , _refLocationName  :: Maybe Text
    , _refStreaming     :: !Bool
    , _refResultWrapper :: Maybe Text
    , _refWrapper       :: !Bool
    , _refFlattened     :: !Bool
    , _refException     :: !Bool
    , _refFault         :: !Bool
    } deriving (Eq, Show)

makeLenses ''Ref

instance FromJSON Ref where
    parseJSON = withObject "ref" $ \o -> Ref
        <$> o .:  "shape"
        <*> o .:? "documentation"
        <*> o .:? "location"
        <*> o .:? "locationName"
        <*> o .:? "streaming" .!= False
        <*> o .:? "resultWrapper"
        <*> o .:? "wrapper"   .!= False
        <*> o .:? "flattened" .!= False
        <*> o .:? "exception" .!= False
        <*> o .:? "fault"     .!= False

data List = List
    { _listDocumentation :: Maybe Text
    , _listMember        :: Ref
    , _listMin           :: !Int
    , _listMax           :: Maybe Int
    , _listFlattened     :: !Bool
    , _listLocationName  :: Maybe Text
    } deriving (Eq, Show)

data Map = Map
    { _mapDocumentation :: Maybe Text
    , _mapKey           :: Ref
    , _mapValue         :: Ref
    , _mapMin           :: !Int
    , _mapMax           :: Maybe Int
    , _mapFlattened     :: !Bool
    } deriving (Eq, Show)

data Struct = Struct
    { _structDocumentation :: Maybe Text
    , _structRequired      :: [Text]
    , _structMembers       :: OrdMap Ref
    , _structPayload       :: Maybe Text
    , _structXmlNamespace  :: Maybe XMLNS
    , _structException     :: !Bool
    , _structFault         :: !Bool
    } deriving (Eq, Show)

data Chars = Chars
    { _charsDocumentation :: Maybe Text
    , _charsMin           :: !Int
    , _charsMax           :: Maybe Int
    , _charsPattern       :: Maybe Text
    , _charsXmlAttribute  :: !Bool
    , _charsLocationName  :: Maybe Text
    , _charsSensitive     :: !Bool
    } deriving (Eq, Show)

data Enum = Enum
    { _enumDoc          :: Maybe Text
    , _enumLocationName :: Maybe Text
    , _enumEnum         :: [Text]
    }

data Blob = Blob
    { _blobDoc       :: Maybe Text
    , _blobSensitive :: !Bool
    , _blobStreaming :: !Bool
    } deriving (Eq, Show)

data Boolean = Boolean
    { _boolDocumentation :: Maybe Text
    , _boolBox           :: !Bool
    } deriving (Eq, Show)

data Time = Time
    { _timeDocumentation   :: Maybe Text
    , _timeTimestampFormat :: !Timestamp
    } deriving (Eq, Show)

data Number a = Number
    { _numDocumentation :: Maybe Text
    , _numMin           :: !a
    , _numMax           :: Maybe a
    , _numBox           :: !Bool
    } deriving (Eq, Show)

-- | The sum of all possible types.
data Shape
    = SList   List
    | SMap    Map
    | SStruct Struct
    | SString Chars
    | SBlob   Blob
    | SBool   Boolean
    | STime   Time
    | SInt    (Number Int)
    | SDouble (Number Double)
    | SLong   (Number Integer)
      deriving (Eq, Show)

makePrisms ''Shape

-- | Applicable HTTP components for an operation.
data HTTP = HTTP
    { _httpMethod     :: !Method
    , _httpRequestUri :: !URI
    , _httpStatus     :: Maybe Int
    } deriving (Eq, Show)

deriveFromJSON spinal ''HTTP

-- | An individual service opereration.
data Operation = Operation
    { _operName             :: !Text
    , _operDocumentation    :: Maybe Text
    , _operDocumentationUrl :: Maybe Text
    , _operHttp             :: !HTTP
    , _operInput            :: Maybe Ref
    , _operOutput           :: Maybe Ref
    , _operErrors           :: [Ref]
    } deriving (Eq, Show)

makeLenses ''Operation

instance FromJSON Operation where
    parseJSON = withObject "operation" $ \o -> Operation
        <$> o .:  "name"
        <*> o .:? "documentation"
        <*> o .:? "documentationUrl"
        <*> o .:  "http"
        <*> o .:? "input"
        <*> o .:? "output"
        <*> o .:? "errors" .!= mempty

-- | Top-level service metadata.
data Metadata = Metadata
    { _metaServiceFullName     :: !Text
    , _metaServiceAbbreviation :: !Text
    , _metaApiVersion          :: !Text
    , _metaEndpointPrefix      :: !Text
    , _metaGlobalEndpoint      :: Maybe Text
    , _metaSignatureVersion    :: !Signature
    , _metaXmlNamespace        :: Maybe Text
    , _metaTargetPrefix        :: Maybe Text
    , _metaJsonVersion         :: Maybe Text
    , _metaTimestampFormat     :: Maybe Timestamp
    , _metaChecksumFormat      :: Maybe Checksum
    , _metaProtocol            :: !Protocol
    } deriving (Eq, Show)

makeClassy ''Metadata
deriveFromJSON spinal ''Metadata

data Service = Service
    { _svcMetadata         :: !Metadata
    , _svcDocumentation    :: !Text
    , _svcDocumentationUrl :: !Text
    , _svcOperations       :: HashMap Text Operation
    , _svcShapes           :: HashMap Text Object
    , _svcOverride         :: !Override
    , _svcName             :: !Text
    } deriving (Eq, Show)

makeLenses ''Service

instance HasMetadata Service where metadata = svcMetadata
instance HasOverride Service where override = svcOverride

instance FromJSON (Text -> Service) where
    parseJSON = withObject "service" $ \o -> Service
        <$> o .:  "metadata"
        <*> o .:  "documentation"
        <*> o .:? "documentationUrl" .!= mempty -- FIXME: temporarily defaulted
        <*> o .:  "operations"
        <*> o .:  "shapes"
        <*> parseJSON (Object o)
