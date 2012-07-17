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
    , open
    , close
    , push
    , hSend
    ) where

import Control.Monad                  (unless)
import Network.Socket                 hiding (send)
import Network.Socket.ByteString.Lazy (send)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Socket handle
data Handle = Handle Socket SockAddr deriving (Show)

type Group = BS.ByteString

type Bucket = BS.ByteString

type Value = BS.ByteString

-- data Metric =
--       Counter Group Bucket Value
--     | Gauge Group Bucket Value
--     | Timer Group Bucket Value
--       deriving (Show)

data MetricType = Counter | Gauge | Timer deriving (Show)

data Metric = Metric MetricType Group Bucket Value deriving (Show)

class MetricSink a where
    encode :: Metric -> a -> IO BL.ByteString

--
-- API
--

-- | Create a new unconnected socket handle for UDP communication
open :: SocketType -> String -> String -> IO Handle
open typ host port = do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
    sock     <- socket (addrFamily addr) typ defaultProtocol
    return $ Handle sock (addrAddress addr)

-- | Close a socket handle
close :: Handle -> IO ()
close (Handle sock _) = sClose sock

-- | Push an encoded metric to the specified socket handle
push :: MetricSink a => Metric -> a -> Handle -> IO ()
push m e h = flip hSend h =<< encode m e

-- | Direct access for writing a bytestring to the socket handle
hSend :: BL.ByteString -> Handle -> IO ()
hSend bstr (Handle sock addr) | BL.null bstr = return ()
                              | otherwise    = do
    sIsConnected sock >>= \b -> unless b $ connect sock addr
    _ <- send sock bstr
    return ()
