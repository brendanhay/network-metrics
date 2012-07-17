-- |
-- Module      : Network.Metrics.Internal
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metrics.Internal (
    -- * Exported types
      Handle(..)
    , Group
    , Bucket
    , Value
    , MetricType(..)
    , Metric(..)
    , MetricSink(..)

    -- * Socket Handle operations
    , hOpen
    , hClose
    , hPush
    ) where

import Control.Monad                  (unless)
import Network.Socket                 hiding (send)
import Network.Socket.ByteString.Lazy (send)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Socket handle
data Handle = Handle Socket SockAddr deriving (Show)

-- | Metric group
type Group = BS.ByteString

-- | Metric bucket
type Bucket = BS.ByteString

-- | Metric value
type Value = BS.ByteString

-- | Metric type
data MetricType = Counter | Gauge | Timer deriving (Show)

-- | Concrete metric data type
data Metric = Metric MetricType Group Bucket Value deriving (Show)

-- | Describes a sink resource which is held open for metric emission
class MetricSink a where
    push  :: Metric -> a -> IO ()
    close :: a -> IO ()

--
-- API
--

-- | Create a new unconnected socket handle for UDP communication
hOpen :: SocketType -> String -> String -> IO Handle
hOpen typ host port = do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
    sock     <- socket (addrFamily addr) typ defaultProtocol
    return $ Handle sock (addrAddress addr)

-- | Close a socket handle
hClose :: Handle -> IO ()
hClose (Handle sock _) = sClose sock

-- | Direct access for writing a bytestring to the socket handle
hPush :: BL.ByteString -> Handle -> IO ()
hPush bstr (Handle sock addr) | BL.null bstr = return ()
                              | otherwise    = do
    sIsConnected sock >>= \b -> unless b $ connect sock addr
    _ <- send sock bstr
    return ()
