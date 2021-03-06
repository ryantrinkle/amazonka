{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns a data key encrypted by a customer master key without the plaintext
-- copy of that key. Otherwise, this API functions exactly like 'GenerateDataKey'.
-- You can use this API to, for example, satisfy an audit requirement that an
-- encrypted key be made available without exposing the plaintext copy of that
-- key.
--
-- <http://docs.aws.amazon.com/kms/latest/APIReference/API_GenerateDataKeyWithoutPlaintext.html>
module Network.AWS.KMS.GenerateDataKeyWithoutPlaintext
    (
    -- * Request
      GenerateDataKeyWithoutPlaintext
    -- ** Request constructor
    , generateDataKeyWithoutPlaintext
    -- ** Request lenses
    , gdkwpEncryptionContext
    , gdkwpGrantTokens
    , gdkwpKeyId
    , gdkwpKeySpec
    , gdkwpNumberOfBytes

    -- * Response
    , GenerateDataKeyWithoutPlaintextResponse
    -- ** Response constructor
    , generateDataKeyWithoutPlaintextResponse
    -- ** Response lenses
    , gdkwprCiphertextBlob
    , gdkwprKeyId
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.KMS.Types
import qualified GHC.Exts

data GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintext
    { _gdkwpEncryptionContext :: Map Text Text
    , _gdkwpGrantTokens       :: List "GrantTokens" Text
    , _gdkwpKeyId             :: Text
    , _gdkwpKeySpec           :: Maybe DataKeySpec
    , _gdkwpNumberOfBytes     :: Maybe Nat
    } deriving (Eq, Read, Show)

-- | 'GenerateDataKeyWithoutPlaintext' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwpEncryptionContext' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'gdkwpGrantTokens' @::@ ['Text']
--
-- * 'gdkwpKeyId' @::@ 'Text'
--
-- * 'gdkwpKeySpec' @::@ 'Maybe' 'DataKeySpec'
--
-- * 'gdkwpNumberOfBytes' @::@ 'Maybe' 'Natural'
--
generateDataKeyWithoutPlaintext :: Text -- ^ 'gdkwpKeyId'
                                -> GenerateDataKeyWithoutPlaintext
generateDataKeyWithoutPlaintext p1 = GenerateDataKeyWithoutPlaintext
    { _gdkwpKeyId             = p1
    , _gdkwpEncryptionContext = mempty
    , _gdkwpKeySpec           = Nothing
    , _gdkwpNumberOfBytes     = Nothing
    , _gdkwpGrantTokens       = mempty
    }

-- | Name:value pair that contains additional data to be authenticated during the
-- encryption and decryption processes.
gdkwpEncryptionContext :: Lens' GenerateDataKeyWithoutPlaintext (HashMap Text Text)
gdkwpEncryptionContext =
    lens _gdkwpEncryptionContext (\s a -> s { _gdkwpEncryptionContext = a })
        . _Map

-- | For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#grant_token Grant Tokens>.
gdkwpGrantTokens :: Lens' GenerateDataKeyWithoutPlaintext [Text]
gdkwpGrantTokens = lens _gdkwpGrantTokens (\s a -> s { _gdkwpGrantTokens = a }) . _List

-- | A unique identifier for the customer master key. This value can be a globally
-- unique identifier, a fully specified ARN to either an alias or a key, or an
-- alias name prefixed by "alias/".  Key ARN Example -
-- arn:aws:kms:us-east-1:123456789012:key/12345678-1234-1234-1234-123456789012 Alias ARN Example - arn:aws:kms:us-east-1:123456789012:/alias/MyAliasName
-- Globally Unique Key ID Example - 12345678-1234-1234-123456789012 Alias Name
-- Example - alias/MyAliasName
gdkwpKeyId :: Lens' GenerateDataKeyWithoutPlaintext Text
gdkwpKeyId = lens _gdkwpKeyId (\s a -> s { _gdkwpKeyId = a })

-- | Value that identifies the encryption algorithm and key size. Currently this
-- can be AES_128 or AES_256.
gdkwpKeySpec :: Lens' GenerateDataKeyWithoutPlaintext (Maybe DataKeySpec)
gdkwpKeySpec = lens _gdkwpKeySpec (\s a -> s { _gdkwpKeySpec = a })

-- | Integer that contains the number of bytes to generate. Common values are 128,
-- 256, 512, 1024 and so on. We recommend that you use the 'KeySpec' parameter
-- instead.
gdkwpNumberOfBytes :: Lens' GenerateDataKeyWithoutPlaintext (Maybe Natural)
gdkwpNumberOfBytes =
    lens _gdkwpNumberOfBytes (\s a -> s { _gdkwpNumberOfBytes = a })
        . mapping _Nat

data GenerateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse
    { _gdkwprCiphertextBlob :: Maybe Base64
    , _gdkwprKeyId          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'GenerateDataKeyWithoutPlaintextResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdkwprCiphertextBlob' @::@ 'Maybe' 'Base64'
--
-- * 'gdkwprKeyId' @::@ 'Maybe' 'Text'
--
generateDataKeyWithoutPlaintextResponse :: GenerateDataKeyWithoutPlaintextResponse
generateDataKeyWithoutPlaintextResponse = GenerateDataKeyWithoutPlaintextResponse
    { _gdkwprCiphertextBlob = Nothing
    , _gdkwprKeyId          = Nothing
    }

-- | Ciphertext that contains the wrapped data key. You must store the blob and
-- encryption context so that the key can be used in a future decrypt operation.
--
-- If you are using the CLI, the value is Base64 encoded. Otherwise, it is not
-- encoded.
gdkwprCiphertextBlob :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Base64)
gdkwprCiphertextBlob =
    lens _gdkwprCiphertextBlob (\s a -> s { _gdkwprCiphertextBlob = a })

-- | System generated unique identifier of the key to be used to decrypt the
-- encrypted copy of the data key.
gdkwprKeyId :: Lens' GenerateDataKeyWithoutPlaintextResponse (Maybe Text)
gdkwprKeyId = lens _gdkwprKeyId (\s a -> s { _gdkwprKeyId = a })

instance ToPath GenerateDataKeyWithoutPlaintext where
    toPath = const "/"

instance ToQuery GenerateDataKeyWithoutPlaintext where
    toQuery = const mempty

instance ToHeaders GenerateDataKeyWithoutPlaintext

instance ToJSON GenerateDataKeyWithoutPlaintext where
    toJSON GenerateDataKeyWithoutPlaintext{..} = object
        [ "KeyId"             .= _gdkwpKeyId
        , "EncryptionContext" .= _gdkwpEncryptionContext
        , "KeySpec"           .= _gdkwpKeySpec
        , "NumberOfBytes"     .= _gdkwpNumberOfBytes
        , "GrantTokens"       .= _gdkwpGrantTokens
        ]

instance AWSRequest GenerateDataKeyWithoutPlaintext where
    type Sv GenerateDataKeyWithoutPlaintext = KMS
    type Rs GenerateDataKeyWithoutPlaintext = GenerateDataKeyWithoutPlaintextResponse

    request  = post "GenerateDataKeyWithoutPlaintext"
    response = jsonResponse

instance FromJSON GenerateDataKeyWithoutPlaintextResponse where
    parseJSON = withObject "GenerateDataKeyWithoutPlaintextResponse" $ \o -> GenerateDataKeyWithoutPlaintextResponse
        <$> o .:? "CiphertextBlob"
        <*> o .:? "KeyId"
