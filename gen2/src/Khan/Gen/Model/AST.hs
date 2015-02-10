{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

-- Module      : Khan.Gen.Model.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Model.AST where

import           Data.Aeson                   (FromJSON)
import           Data.Foldable                (Foldable)
import           Data.Text                    (Text)
import           Data.Traversable             (Traversable)
import           Khan.Gen.Model.IndexNotation
import           Khan.Gen.Model.Pager
import           Khan.Gen.Model.Retrier
import           Khan.Gen.Model.URI
import           Khan.Gen.Model.Waiter

data OrdMap a = OrdMap { ordMap :: [(Text, a)] }
    deriving (Eq, Functor, Foldable, Traversable)

data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
      deriving (Eq)

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq)

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq)

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Ord)

data Checksum
    = MD5
    | SHA256
      deriving (Eq)

-- | Top-level service metadata.
data Meta = Meta
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

data XMLNS = XMLNS
    { _xnsPrefix :: !Text
    , _xnsUri    :: !Text
    } deriving (Eq, Show)

-- | The sum of all possible types.
data Type
    = TList   TList
    | TMap    TMap
    | TStruct TStruct
    | TText   TText
    | TBlob   TBlob
    | TBool   TBool
    | TTime   TTime
    | TInt    (TNum Int)
    | TDouble (TNum Double)
    | TLong   (TNum Integer)
      deriving (Eq)

data TList = TList'
    { _listDocumentation :: Maybe Text
    , _listMember        :: TypeRef
    , _listMin           :: Int
    , _listMax           :: Maybe Int
    , _listFlattened     :: Bool
    , _listLocationName  :: Maybe Text
    } deriving (Eq)

data TMap = TMap'
    { _mapDocumentation :: Maybe Text
    , _mapKey           :: TypeRef
    , _mapValue         :: TypeRef
    , _mapMin           :: Int
    , _mapMax           :: Maybe Int
    , _mapFlattened     :: Bool
    } deriving (Eq)

data TStruct = TStruct'
    { _structDocumentation :: Maybe Text
    , _structRequired      :: Maybe [Text]
    , _structMembers       :: OrdMap TypeRef
    , _structPayload       :: Maybe Text
    , _structXmlNamespace  :: Maybe XMLNS
    , _structException     :: Maybe Bool
    , _structFault         :: Maybe Bool
    } deriving (Eq)

data TText = TText'
    { _textDocumentation :: Maybe Text
    , _textMin           :: Int
    , _textMax           :: Maybe Int
    , _textPattern       :: Maybe Text
    , _textXmlAttribute  :: Maybe Bool
    , _textLocationName  :: Maybe Text
    , _textSensitive     :: Maybe Bool
    } deriving (Eq)

data TEnum = TEnum'
    { _enumDoc          :: Maybe Text
    , _enumLocationName :: Maybe Text
    , _enumEnum         :: [Text]
    }

data TBlob = TBlob'
    { _blobDoc       :: Maybe Text
    , _blobSensitive :: Bool
    , _blobStreaming :: Bool
    } deriving (Eq)

data TBool = TBool'
    { _boolDocumentation :: Maybe Text
    , _boolBox           :: Bool
    } deriving (Eq)

data TTime = TTime'
    { _timeDocumentation   :: Maybe Text
    , _timeTimestampFormat :: Timestamp
    } deriving (Eq)

data TNum a = TNum'
    { _numDocumentation :: Maybe Text
    , _numMin           :: a
    , _numMax           :: Maybe a
    , _numBox           :: Bool
    } deriving (Eq)

data Location
    = Headers
    | Header
    | Uri
    | Querystring
    | StatusCode
    | Body
      deriving (Eq)

-- | A reference to a 'Type', plus any additional annotations
-- specific to the point at which the type is de/serialised.
data TypeRef = TypeRef
    { _trefShape         :: Text
    , _trefDocumentation :: Maybe Text
    , _trefLocation      :: Maybe Location
    , _trefLocationName  :: Maybe Text
    , _trefStreaming     :: Bool
    , _trefResultWrapper :: Maybe Text
    , _trefWrapper       :: Bool
    , _trefFlattened     :: Bool
    , _trefException     :: Bool
    , _trefFault         :: Bool
    } deriving (Eq)

-- | Applicable HTTP components for an operation.
data HTTP = HTTP
    { _httpMethod :: Method
    , _httpUri    :: URI
    , _httpStatus :: Maybe Int
    } deriving (Eq)

-- | An individual service operation.
data Operation = Operation
    { _opName             :: Text
    , _opDocumentation    :: Text
    , _opDocumentationUrl :: Maybe Text
    , _opHttp             :: HTTP
    , _opInput            :: Maybe TypeRef
    , _opOutput           :: Maybe TypeRef
    , _opErrors           :: [TypeRef]
    } deriving (Eq)
