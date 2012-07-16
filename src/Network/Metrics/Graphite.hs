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

    -- * Socket Handle operations
    , open
    , emit

    -- * Re-exported from internal
    , I.Handle
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

--
-- API
--

-- | Create a new disconnected socket handle for TCP communication
open :: String -> String -> IO I.Handle
open = I.open Stream

-- | Emit a metric's metadata and value on the specified socket handle
emit :: Metric -> I.Handle -> IO I.Handle
emit Metric{..} handle = do
    time <- getPOSIXTime
    I.emit (encoded time) handle
  where
    encoded n   = BL.fromChunks [bucket, value, timestamp n]
    timestamp n = BS.pack $ show (truncate n :: Integer)