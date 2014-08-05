-- |
-- Module      : Network.Metric.Sink.Graphite
-- Copyright   : (c) 2012-2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    ) where

import Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime)
import Network.Socket          (SocketType(..))
import Network.Metric.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | A handle to a Graphite sink
data Graphite = Graphite (Maybe Host) Handle deriving (Show)

instance Sink Graphite where
    push (Graphite host hd) m = do
        time <- getPOSIXTime
        mapM_ (hPush hd . (`BL.append` "\n") . flat . enc time) (measure m)
      where
        flat s = BL.fromChunks [BS.intercalate " " $ BL.toChunks s]
        enc t (Counter g b v) = put host g b v t
        enc t (Timer g b v)   = put host g b v t
        enc t (Gauge g b v)   = put host g b v t

    close (Graphite _ hd) = hClose hd

--
-- API
--

-- | Open a new Graphite sink
open :: Maybe Host -> HostName -> PortNumber -> IO AnySink
open host = fOpen (Graphite host) Stream

--
-- Private
--

-- | Encode a metric into the Graphite format
put :: Encodable a
    => Maybe Host
    -> Group
    -> Bucket
    -> a
    -> POSIXTime
    -> BL.ByteString
put host group bucket value time =
    BL.fromChunks [key host (safe group) bucket, encode value, timestamp]
  where
    timestamp = BS.pack $ show (truncate time :: Integer)

safe :: BS.ByteString -> BS.ByteString
safe = BS.map fn
  where
    fn ' ' = '.'
    fn c   = c
