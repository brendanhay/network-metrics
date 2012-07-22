-- |
-- Module      : Network.Metric.Sink.Graphite
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric.Sink.Graphite (
    -- * Sink Functions
      Sink(..)
    , open

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    , pack
    ) where

import Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime)
import Network.Socket          (SocketType(..))
import Network.Metric.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | A handle to a Graphite sink
data Graphite = Graphite Handle deriving (Show)

instance Sink Graphite where
    push (Graphite h) m = do
        time <- getPOSIXTime
        mapM_ (hPush h . enc time) (measure m)
      where
        enc t (Counter g b v) = put g b v t
        enc t (Timer g b v)   = put g b v t
        enc t (Gauge g b v)   = put g b v t

    close (Graphite h) = hClose h

--
-- API
--

-- | Open a new Graphite sink
open :: String -> String -> IO AnySink
open = fOpen Graphite Stream

--
-- Private
--

-- | Encode a metric into the Graphite format
put :: Encodable a => Group -> Bucket -> a -> POSIXTime -> BL.ByteString
put group bucket value time =
    BL.fromChunks [key group bucket, encode value, timestamp]
  where
    timestamp = BS.pack $ show (truncate time :: Integer)

