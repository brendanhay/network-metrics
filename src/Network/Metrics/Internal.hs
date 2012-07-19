{-# LANGUAGE ExistentialQuantification #-}

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
    -- * Exported Types
      Handle(..)
    , Group
    , Bucket
    , Metric(..)
    , MetricSink(..)

    -- * Exported Type Classes
    , Encodable(..)
    , Sink(..)

    -- * Socket Handle Functions
    , hOpen
    , hClose
    , hPush
    ) where

import Control.Monad                  (unless, void)
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

data Metric a =
      Counter Group Bucket Int
    | Timer Group Bucket Double
    | Gauge Group Bucket a
      deriving (Show)

-- | A Metric's value
class Encodable a where
    -- | Encode the value as a bytestring
    encode :: Typeable a => a -> BS.ByteString

instance Encodable Int where
    encode = BS.pack . show

instance Encodable Double where
    encode = BS.pack . show

instance Encodable String where
    encode = BS.pack

-- | Sink resource to write metrics to
class Sink a where
    -- | Write a metric to the sink.
    push  :: Encodable b => a -> Metric b -> IO ()

    -- | Close the sink - subsequent writes will throw an error.
    close :: a -> IO ()

    -- | Effeciently write multiple metrics simultaneously.
    mpush :: Encodable b => a -> [Metric b] -> IO ()
    mpush s = void . mapM (push s)

-- | Existential sink type
data MetricSink = forall a. Sink a => MetricSink a

-- | Existential sink instance
instance Sink MetricSink where
    push  (MetricSink s) = push s
    mpush (MetricSink s) = mpush s
    close (MetricSink s) = close s

--
-- API
--

-- | Create a new socket handle (in a disconnected state) for UDP communication
hOpen :: SocketType -> String -> String -> IO Handle
hOpen typ host port = do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)
    sock     <- socket (addrFamily addr) typ defaultProtocol
    return $ Handle sock (addrAddress addr)

-- | Close a socket handle
hClose :: Handle -> IO ()
hClose (Handle sock _) = sClose sock

-- | Direct access for writing a bytestring to a socket handle
hPush :: Handle -> BL.ByteString -> IO ()
hPush (Handle sock addr) bstr | BL.null bstr = return ()
                              | otherwise    = do
    sIsConnected sock >>= \b -> unless b $ connect sock addr
    _ <- send sock bstr
    return ()
