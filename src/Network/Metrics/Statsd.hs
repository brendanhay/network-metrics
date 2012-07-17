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
    -- * Socket Handle operations
      open

    -- * Network.Metrics.Internal re-exported types
    , Group
    , Bucket
    , Value
    , MetricType(..)
    , Metric(..)
    , MetricSink(push)

    -- * Network.Metrics.Internal operations
    , close
    ) where

import Control.Monad  (liftM)
import Network.Socket (SocketType(..))
import System.Random  (randomRIO)
import Network.Metrics.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data StatsdMetric = StatsdMetric
    { type'  :: MetricType
    , bucket :: BS.ByteString
    , value  :: BS.ByteString
    , rate   :: Double
    } deriving (Show)

data Sampled = Sampled | Exact | Ignore

data Statsd = Statsd Handle

instance MetricSink Statsd where
    push m (Statsd h) = encode m >>= flip hPush h
    close  (Statsd h) = hClose h

--
-- API
--

-- | Create a new disconnected socket handle for UDP communication
open :: String -> String -> IO Statsd
open host port = liftM Statsd (hOpen Datagram host port)

--
-- Private
--

encode :: Metric -> IO BL.ByteString
encode (Metric t g b v) = liftM bstr (randomRIO (0.0, 1.0))
  where
    metric = StatsdMetric t (BS.concat [g, ".", b]) v 1.0
    bstr   = BL.fromChunks . chunks metric . sample (rate metric)

sample :: Double -> Double -> Sampled
sample rate rand | rate < 1.0 && rand <= rate = Sampled
                 | rate == 1.0                = Exact
                 | otherwise                  = Ignore

chunks :: StatsdMetric -> Sampled -> [BS.ByteString]
chunks StatsdMetric{..} sampled = case sampled of
    Sampled -> base ++ ["@", BS.pack $ show rate]
    Exact   -> base
    Ignore  -> []
  where
    base = [bucket, ":", value, "|", suffix type']

suffix :: MetricType -> BS.ByteString
suffix typ = case typ of
    Counter -> "c"
    Gauge   -> "g"
    Timer   -> "ms"
