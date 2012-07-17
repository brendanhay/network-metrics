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
    -- * Exported types
      Statsd(..)

    -- * Socket Handle operations
    , open

    -- * Re-exported from internal
    , I.Handle
    , I.push
    , I.close
    ) where

import Control.Monad  (liftM)
import Network.Socket
import System.Random  (randomRIO)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Metrics.Internal   as I

data Metric = Metric
    { type'  :: I.MetricType
    , bucket :: BS.ByteString
    , value  :: BS.ByteString
    , rate   :: Double
    } deriving (Show)

data Sampled = Sampled | Exact | Ignore

data Statsd = Statsd

instance I.MetricSink Statsd where
    encode m _ = liftM fn (randomRIO (0.0, 1.0))
      where
        fn = BL.fromChunks . chunks x . sample (rate x)
        x  = conv m

--
-- API
--

-- | Create a new disconnected socket handle for UDP communication
open :: String -> String -> IO I.Handle
open = I.open Datagram

--
-- Private
--

conv :: I.Metric -> Metric
conv (I.Metric t g b v) = Metric t (BS.concat [g, ".", b]) v 1.0

sample :: Double -> Double -> Sampled
sample rate rand | rate < 1.0 && rand <= rate = Sampled
                 | rate == 1.0                = Exact
                 | otherwise                  = Ignore

chunks :: Metric -> Sampled -> [BS.ByteString]
chunks Metric{..} sampled = case sampled of
    Sampled -> base ++ ["@", BS.pack $ show rate]
    Exact   -> base
    Ignore  -> []
  where
    base = [bucket, ":", value, "|", suffix type']

suffix :: I.MetricType -> BS.ByteString
suffix typ = case typ of
    I.Counter -> "c"
    I.Gauge   -> "g"
    I.Timer   -> "ms"
