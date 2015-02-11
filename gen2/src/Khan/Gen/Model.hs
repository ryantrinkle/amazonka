{-# LANGUAGE TemplateHaskell #-}

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

import           Control.Lens
import           Data.Aeson               (FromJSON)
import           Data.HashMap.Strict      (HashMap)
import           Data.Text                (Text)
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
      deriving (Eq)

deriveJSON ''Method

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq)

deriveJSON ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq)

deriveJSON ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Ord)

deriveJSON ''Timestamp

data Checksum
    = MD5
    | SHA256
      deriving (Eq)

deriveJSON ''Checksum

data Location
    = Headers
    | Header
    | Uri
    | Querystring
    | StatusCode
    | Body
      deriving (Eq)

deriveJSON ''Location

data XMLNS = XMLNS
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

makeLenses ''XMLNS
deriveJSON ''XMLNS

-- | A reference to a 'Shape', plus any additional annotations
-- specific to the point at which the type is de/serialised.
data Ref = Ref
    { _refShape         :: Text
    , _refDocumentation :: Maybe Text
    , _refLocation      :: Maybe Location
    , _refLocationName  :: Maybe Text
    , _refStreaming     :: Bool
    , _refResultWrapper :: Maybe Text
    , _refWrapper       :: Bool
    , _refFlattened     :: Bool
    , _refException     :: Bool
    , _refFault         :: Bool
    } deriving (Eq)

makeLenses ''Ref
deriveJSON ''Ref

data List = List
    { _listDocumentation :: Maybe Text
    , _listMember        :: Ref
    , _listMin           :: Int
    , _listMax           :: Maybe Int
    , _listFlattened     :: Bool
    , _listLocationName  :: Maybe Text
    } deriving (Eq)

data Map = Map
    { _mapDocumentation :: Maybe Text
    , _mapKey           :: Ref
    , _mapValue         :: Ref
    , _mapMin           :: Int
    , _mapMax           :: Maybe Int
    , _mapFlattened     :: Bool
    } deriving (Eq)

data Struct = Struct
    { _structDocumentation :: Maybe Text
    , _structRequired      :: [Text]
    , _structMembers       :: OrdMap Ref
    , _structPayload       :: Maybe Text
    , _structXmlNamespace  :: Maybe XMLNS
    , _structException     :: Bool
    , _structFault         :: Bool
    } deriving (Eq)

data Chars = Chars
    { _charsDocumentation :: Maybe Chars
    , _charsMin           :: Int
    , _charsMax           :: Maybe Int
    , _charsPattern       :: Maybe Chars
    , _charsXmlAttribute  :: Bool
    , _charsLocationName  :: Maybe Chars
    , _charsSensitive     :: Bool
    } deriving (Eq)

data Enum = Enum
    { _enumDoc          :: Maybe Text
    , _enumLocationName :: Maybe Text
    , _enumEnum         :: [Text]
    }

data Blob = Blob
    { _blobDoc       :: Maybe Text
    , _blobSensitive :: Bool
    , _blobStreaming :: Bool
    } deriving (Eq)

data Boolean = Boolean
    { _boolDocumentation :: Maybe Text
    , _boolBox           :: Bool
    } deriving (Eq)

data Time = Time
    { _timeDocumentation   :: Maybe Text
    , _timeTimestampFormat :: Timestamp
    } deriving (Eq)

data Number a = Number
    { _numDocumentation :: Maybe Text
    , _numMin           :: a
    , _numMax           :: Maybe a
    , _numBox           :: Bool
    } deriving (Eq)

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
      deriving (Eq)

makePrisms ''Shape

-- | Applicable HTTP components for an operation.
data HTTP = HTTP
    { _httpMethod :: Method
    , _httpUri    :: URI
    , _httpStatus :: Maybe Int
    } deriving (Eq)

deriveJSON ''HTTP

-- | An individual service opereration.
data Operation = Operation
    { _operName             :: Text
    , _operDocumentation    :: Text
    , _operDocumentationUrl :: Maybe Text
    , _operHttp             :: HTTP
    , _operInput            :: Maybe Ref
    , _operOutput           :: Maybe Ref
    , _operErrors           :: [Ref]
    } deriving (Eq)

makeLenses ''Operation
deriveJSON ''Operation

-- | Top-level service metadata.
data Metadata = Metadata
    { _metaServiceFullName     :: Text
    , _metaServiceAbbreviation :: Text
    , _metaApiVersion          :: Text
    , _metaEndpointPrefix      :: Text
    , _metaGlobalEndpoint      :: Maybe Text
    , _metaSignatureVersion    :: Signature
    , _metaXmlNamespace        :: Maybe Text
    , _metaTargetPrefix        :: Maybe Text
    , _metaJsonVersion         :: Maybe Text
    , _metaTimestampFormat     :: Maybe Timestamp
    , _metaChecksumFormat      :: Maybe Checksum
    , _metaProtocol            :: Protocol
    } deriving (Eq)

makeLenses ''Metadata
deriveJSON ''Metadata

data Service = Service
    { _svcName             :: Text
    , _svcFullName         :: Text
    , _svcLibraryName      :: Text
    , _svcMetadata         :: Metadata
    , _svcDocumentation    :: Text
    , _svcDocumentationUrl :: Text
    , _svcOperations       :: HashMap Text Operation
    , _svcShapes           :: HashMap Text Shape
    } deriving (Eq)

makeLenses ''Service
