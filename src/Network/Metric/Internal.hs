{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Network.Metric.Internal
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric.Internal (
    -- * Exported Types
      Handle(..)
    , Host
    , Group
    , Bucket
    , Metric(..)

    -- * Existential Types
    , AnyMeasurable(..)
    , AnySink(..)

    -- * Exported Type Classes
    , Measurable(..)
    , Encodable(..)
    , Sink(..)

    -- * General Functions
    , counter
    , timer
    , gauge
    , key

    -- * Socket Handle Functions
    , fOpen
    , hOpen
    , hClose
    , hPush

    -- * Re-exports
    , HostName
    , PortNumber(..)
    ) where

import Control.Monad                  (liftM, unless)
import Data.Typeable                  (Typeable)
import Network.Socket                 hiding (send)
import Network.Socket.ByteString.Lazy (send)
import Text.Printf                    (printf)

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

-- | Socket handle
data Handle = Handle Socket SockAddr deriving (Show)

-- | Metric host
type Host = BS.ByteString

-- | Metric group
type Group = BS.ByteString

-- | Metric bucket
type Bucket = BS.ByteString

data Metric
    = Counter Group Bucket Integer
    | Timer Group Bucket Double
    | Gauge Group Bucket Double
      deriving (Show)

--
-- Type Classes
--

-- | Measure a type for metrics
class Measurable a where
    -- | Convert a measurable instance from a host into a list of metrics
    measure :: a -> [Metric]

-- | Metric value to be encoded
class (Show a, Typeable a) => Encodable a where
    -- | Encode the value as a bytestring
    encode :: a -> BS.ByteString

-- | Sink resource to write metrics to
class Sink a where
    -- | Write a metric to the sink.
    push  :: Measurable b => a -> b -> IO ()
    -- | Close the sink - subsequent writes will throw an error.
    close :: a -> IO ()

--
-- Existential Types
--

-- | Any instance of the Measurable type class
data AnyMeasurable = forall a. Measurable a => AnyMeasurable a

-- | Any instance of the Sink type class
data AnySink = forall a. Sink a => AnySink a

--
-- Instances
--

instance Measurable AnyMeasurable where
    measure (AnyMeasurable m) = measure m

instance Measurable Metric where
    measure = flip (:) [] . id

instance Encodable Int where
    encode = BS.pack . show

instance Encodable Integer where
    encode = BS.pack . show

instance Encodable Double where
    encode = BS.pack . printf "%.8f"

instance Encodable String where
    encode = BS.pack

-- | Existential sink instance
instance Sink AnySink where
    push  (AnySink s) = push s
    close (AnySink s) = close s

--
-- API
--

counter :: Group -> BS.ByteString -> Bucket -> Integer -> Metric
counter g e b v = Counter g (bucket e b) v

timer :: Group -> BS.ByteString -> Bucket -> Double -> Metric
timer g e b v = Timer g (bucket e b) v

gauge :: Group -> BS.ByteString -> Bucket -> Double -> Metric
gauge g b e v = Gauge g (bucket e b) v

-- | Combine a Host, Group and Bucket into a single key
key :: Host -> Group -> Bucket -> BS.ByteString
key h g b = BS.intercalate "." [h, g, b]

-- | Helper to curry a constructor function for a sink
fOpen :: Sink a
      => (Handle -> a)
      -> SocketType
      -> HostName
      -> PortNumber
      -> IO AnySink
fOpen ctor typ host port = liftM (AnySink . ctor) (hOpen typ host port)

-- | Create a new socket handle (in a disconnected state) for UDP communication
hOpen :: SocketType -> HostName -> PortNumber -> IO Handle
hOpen typ host (PortNum port) = do
    (addr:_) <- getAddrInfo Nothing (Just host) (Just $ show port)
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

--
-- Private
--

bucket :: BS.ByteString -> Bucket -> BS.ByteString
bucket e b = BS.concat [e, ".", b]
