{-# LANGUAGE TemplateHaskell #-}

-- Module      : Khan.Gen.Model.Waiter
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Khan.Gen.Model.Waiter where

import           Control.Lens
import           Data.Aeson   (FromJSON)
import           Data.Text    (Text)

data Match
    = MatchPath
    | MatchPathAll
    | MatchPathAny
    | MatchStatus
    | MatchError
      deriving (Eq)

data State
    = StateRetry
    | StateSuccess
    | StateFailure
      deriving (Eq)

data Expected
    = ExpectStatus Int
    | ExpectText   Text
    | ExpectCtor   Text
      deriving (Eq)

data Notation
    = Indexed Text Notation
    | Nested  Text Notation
    | Access  Text
      deriving (Eq)

data Acceptor = Acceptor
    { _acceptExpected :: Expected
    , _acceptMatcher  :: Match
    , _acceptState    :: State
    , _acceptArgument :: Maybe Notation
    } deriving (Eq)

data Wait = Wait
    { _waitDelay       :: Int
    , _waitMaxAttempts :: Int
    , _waitOperation   :: Text
    , _waitAcceptors   :: [Acceptor]
    } deriving (Eq)
