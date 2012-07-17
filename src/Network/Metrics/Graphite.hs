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

import Control.Monad            (liftM)
import Network.Socket
import Data.Time.Clock.POSIX
import Network.Metrics.Internal

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Graphite = Graphite Handle

instance MetricSink Graphite where
    push m (Graphite h) = encode m >>= flip hPush h
    close  (Graphite h) = hClose h
--
-- API
--

-- | Create a new disconnected socket handle for TCP communication
open :: String -> String -> IO Graphite
open host port = liftM Graphite (hOpen Stream host port)

--
-- Private
--

encode :: Metric -> IO BL.ByteString
encode (Metric _ g b v) = liftM bstr getPOSIXTime
  where
    bucket      = BS.concat [g, ".", b]
    timestamp n = BS.pack $ show (truncate n :: Integer)
    bstr n      = BL.fromChunks [bucket, v, timestamp n]

