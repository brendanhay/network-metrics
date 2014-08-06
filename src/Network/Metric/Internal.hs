{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Network.Metric.Internal
-- Copyright   : (c) 2012-2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , key

    -- * Socket Handle Functions
    , fOpen
    , hOpen
    , hClose
    , hPush

    -- * Re-exports
    , S.HostName
    , S.PortNumber
    ) where

import Control.Monad (liftM, unless)
import Data.Typeable (Typeable)
import Data.Word     (Word16)
import Text.Printf   (printf)

import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy.Char8     as BL
import qualified Network.Socket                 as S
import qualified Network.Socket.ByteString.Lazy as SBL

-- | Socket handle
data Handle = Handle S.Socket S.SockAddr deriving (Show)

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
      deriving (Show, Eq)

--
-- Type Classes
--

-- | Measure a type for a collection of metrics
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
    measure = flip (:) []

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

-- | Combine a Host, Group and Bucket into a single key
key :: Maybe Host -> Group -> Bucket -> BS.ByteString
key (Just h) g b = BS.intercalate "." [h, g, b]
key Nothing g b  = BS.intercalate "." [g, b]

-- | Helper to curry a constructor function for a sink
fOpen :: Sink a
      => (Handle -> a)
      -> S.SocketType
      -> S.HostName
      -> S.PortNumber
      -> IO AnySink
fOpen ctor typ host port = liftM (AnySink . ctor) (hOpen typ host port)

-- | Create a new socket handle (in a disconnected state) for UDP communication
hOpen :: S.SocketType -> S.HostName -> S.PortNumber -> IO Handle
hOpen typ host port = do
    (addr:_) <- S.getAddrInfo Nothing (Just host) (Just . show . p2w $ port)
    sock     <- S.socket (S.addrFamily addr) typ S.defaultProtocol
    return $ Handle sock (S.addrAddress addr)
  where
    p2w :: S.PortNumber -> Word16
    p2w = fromIntegral

-- | Close a socket handle
hClose :: Handle -> IO ()
hClose (Handle sock _) = S.sClose sock

-- | Direct access for writing a bytestring to a socket handle
hPush :: Handle -> BL.ByteString -> IO ()
hPush (Handle sock addr) bstr | BL.null bstr = return ()
                              | otherwise    = do
    S.sIsConnected sock >>= \b -> unless b $ S.connect sock addr
    _ <- SBL.send sock bstr
    return ()
