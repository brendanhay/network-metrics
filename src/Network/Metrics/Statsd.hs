-- |
-- Module      : Network.Metrics.Statsd
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metrics.Statsd (
    -- * Sink Functions
      open
    , MetricSink(push, close)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    ) where

import Control.Monad            (liftM)
import Network.Socket           (SocketType(..))
import System.Random            (randomRIO)
import Network.Metrics.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | The sample status of a metric
data Sampled = Sampled | Exact | Ignore

-- | A handle to a Statsd sink
data Statsd = Statsd Handle deriving (Show)

instance MetricSink Statsd where
    push  (Statsd h) m  = encode m >>= hPush h
    close (Statsd h)    = hClose h

--
-- API
--

-- | Open a new Statsd sink
open :: String -> String -> IO Sink
open host port = liftM (Sink . Statsd) (hOpen Datagram host port)

--
-- Private
--

-- | Encode a metric into the Statsd format
encode :: MetricValue a => Metric a -> IO BL.ByteString
encode (Counter g b v) = put g b v "c" 1.0
encode (Gauge g b v)   = put g b v "g" 1.0
encode (Timer g b v)   = put g b v "ms" 1.0

-- | TODO: Currently statsd sampling is not exposed via the global metric type
put :: MetricValue a
    => Group
    -> Bucket
    -> a
    -> BS.ByteString
    -> Double
    -> IO BL.ByteString
put g b v t rate = liftM bstr (randomRIO (0.0, 1.0))
  where
    bucket = BS.concat [g, ".", b]
    base   = [bucket, ":", enc v, "|", t]
    bstr n = BL.fromChunks $ case sample rate n of
        Sampled -> base ++ ["@", BS.pack $ show rate]
        Exact   -> base
        Ignore  -> []

sample :: Double -> Double -> Sampled
sample rate rand | rate < 1.0 && rand <= rate = Sampled
                 | rate == 1.0                = Exact
                 | otherwise                  = Ignore
