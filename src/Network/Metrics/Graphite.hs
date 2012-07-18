-- |
-- Module      : Network.Metrics.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metrics.Graphite (
    -- * Sink Functions
      open
    , Sink(push, close)

    -- * Re-exports
    , Group
    , Bucket
    , Value
    , MetricType(..)
    , Metric(..)
    ) where

import Control.Monad            (liftM)
import Network.Socket
import Data.Time.Clock.POSIX
import Network.Metrics.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | A handle to a Graphite sink
data Graphite = Graphite Handle deriving (Show)

instance Sink Graphite where
    push m (Graphite h) = encode m >>= flip hPush h
    close  (Graphite h) = hClose h

--
-- API
--

-- | Open a new Graphite sink
open :: String -> String -> IO Graphite
open host port = liftM Graphite (hOpen Stream host port)

--
-- Private
--

-- | Encode a metric into the Graphite format
encode :: Metric -> IO BL.ByteString
encode (Metric _ g b v) = liftM bstr getPOSIXTime
  where
    bucket      = BS.concat [g, ".", b]
    timestamp n = BS.pack $ show (truncate n :: Integer)
    bstr n      = BL.fromChunks [bucket, v, timestamp n]

