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
    , MetricSink(push, close)

    -- * Re-exports
    , Group
    , Bucket
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

instance MetricSink Graphite where
    push  (Graphite h) m  = encode m >>= hPush h
    close (Graphite h)    = hClose h

--
-- API
--

-- | Open a new Graphite sink
open :: String -> String -> IO Sink
open host port = liftM (Sink . Graphite) (hOpen Stream host port)

--
-- Private
--

-- | Encode a metric into the Graphite format
encode :: MetricValue a => Metric a -> IO BL.ByteString
encode (Counter g b v) = put g b v
encode (Gauge g b v)   = put g b v
encode (Timer g b v)   = put g b v

put :: MetricValue a => Group -> Bucket -> a -> IO BL.ByteString
put g b v = liftM bstr getPOSIXTime
  where
    bucket      = BS.concat [g, ".", b]
    timestamp n = BS.pack $ show (truncate n :: Integer)
    bstr n      = BL.fromChunks [bucket, enc v, timestamp n]

