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
    -- * Exported types
      Metric(..)
    , Graphite(..)

    -- * Socket Handle operations
    , open

    -- * Re-exported from internal
    , I.Handle
    , I.push
    , I.close
    ) where

import Network.Socket
import Data.Time.Clock.POSIX

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Metrics.Internal   as I

data Metric = Metric
    { bucket :: BS.ByteString
    , value  :: BS.ByteString
    } deriving (Show)

data Graphite = Graphite

instance I.MetricSink Graphite where
    encode m e = getPOSIXTime >>= return . fn
      where
        Metric{..} = Metric "bucket" "value" -- conv m
        fn n = BL.fromChunks [bucket, value, ts n]
        ts n = BS.pack $ show (truncate n :: Integer)

--
-- API
--

-- | Create a new disconnected socket handle for TCP communication
open :: String -> String -> IO I.Handle
open = I.open Stream
