-- |
-- Module      : Monitor.Statsd
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Monitor.Statsd (
    -- * Exported types
      MetricType
    , Metric(..)

    -- * Socket Handle operations
    , open
    , emit

    -- * Re-exported from internal
    , I.Handle
    , I.close
    ) where

import Network.Socket
import System.Random (randomRIO)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Vigil.Internal             as I

data MetricType = Counter | Timer | Gauge deriving (Show)

data Metric = Metric
    { metricType   :: MetricType
    , metricBucket :: BS.ByteString
    , metricValue  :: Integer
    , metricRate   :: Double
    } deriving (Show)

data Sampled = Sampled | Exact | Ignore

--
-- API
--

-- | Create a new disconnected socket handle for UDP communication
open :: String -> String -> IO I.Handle
open = I.open Datagram

-- | Emit a metric's metadata and value on the specified socket handle
emit :: Metric -> I.Handle -> IO I.Handle
emit metric handle = do
    rand <- randomRIO (0.0, 1.0)
    I.emit (parts rand) handle
  where
    parts = BL.fromChunks . components metric . sample (metricRate metric)

--
-- Binary Encoding
--

sample :: Double -> Double -> Sampled
sample rate rand | rate < 1.0 && rand <= rate = Sampled
                 | rate == 1.0                = Exact
                 | otherwise                  = Ignore

components :: Metric -> Sampled -> [BS.ByteString]
components Metric{..} sampled = case sampled of
    Sampled -> base ++ ["@", BS.pack $ show metricRate]
    Exact   -> base
    Ignore  -> []
  where
    base = [metricBucket, ":", BS.pack $ show metricValue, "|", suffix metricType]

suffix :: MetricType -> BS.ByteString
suffix typ = case typ of
    Counter -> "c"
    Gauge   -> "g"
    Timer   -> "ms"
