-- |
-- Module      : Network.Metric.Sink.Statsd
-- Copyright   : (c) 2012-2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric.Sink.Statsd (
    -- * Sink Functions
      Sink(..)
    , open

    -- * Re-exports
    , Group
    , Bucket
    , AnySink(..)
    ) where

import Control.Monad            (liftM)
import Network.Socket           (SocketType(..))
import System.Random            (randomRIO)
import Network.Metric.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | The sample status of a metric
data Sampled = Sampled | Exact | Ignore

-- | A handle to a Statsd sink
data Statsd = Statsd Host Handle deriving (Show)

instance Sink Statsd where
    push (Statsd host hd) m = mapM enc (measure m) >>= mapM_ (hPush hd)
      where
        enc (Counter g b v) = put host g b v "c" 1.0
        enc (Gauge g b v)   = put host g b v "g" 1.0
        enc (Timer g b v)   = put host g b v "ms" 1.0

    close (Statsd _ hd) = hClose hd

--
-- API
--

-- | Open a new Statsd sink
open :: Host -> HostName -> PortNumber -> IO AnySink
open host = fOpen (Statsd host) Datagram

--
-- Private
--
-- | Encode a metric into the Statsd format
-- *TODO:* Currently statsd sampling is not exposed via the global metric type
put :: Encodable a
    => Host
    -> Group
    -> Bucket
    -> a
    -> BS.ByteString
    -> Double
    -> IO BL.ByteString
put host group bucket value typ rate = liftM (BL.fromChunks . bstr) (sample rate)
  where
    base = [key host group bucket, ":", encode value, "|", typ]
    bstr Sampled = base ++ ["@", BS.pack $ show rate]
    bstr Exact   = base
    bstr Ignore  = []

-- NOTE: The bizarre type and branching logic of this function is entirely
-- for the purpose of hiding the 'randomRIO' call behind the rate < 1.0
-- condition (which currently will always be False).  This is to sidestep
-- a bug in random <=1.0.1.1 in which the 'randomRIO' function has a space leak.
--
-- There is a far saner, pure version of this function that should probably
-- be resurrected when the bug in the random package is fixed.
sample :: Double -> IO Sampled
sample rate
  | rate < 1.0   = do
      r <- randomRIO (0.0, 1.0)
      return $ if r <= rate then Sampled else Ignore
  | rate == 1.0  = return Exact
  | otherwise    = return Ignore
