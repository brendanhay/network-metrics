-- |
-- Module      : Network.Metrics.Ganglia
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metrics.Ganglia (
    -- * Exported types
      Slope(..)
    , MetricType(..)
    , Metric(..)

    -- * Default constructors
    , defaultMetric

    -- * Socket Handle operations
    , open
    , emit

    -- * Development helpers
    , test

    -- * Re-exported from internal
    , I.Handle
    , I.close
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad      (unless, liftM)
import Data.Binary.Put
import Data.Bits          ((.&.))
import Data.Char          (toLower)
import Data.Data          (Data, Typeable)
import Data.Default       (Default, def)
import Data.Int           (Int32)
import Data.Word          (Word32)
import Network.Socket
import System.Random      (randomRIO)

import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BS
import qualified Network.Metrics.Internal as I

-- | Allows gmetad and the PHP webfrontend to efficiently separate
-- constant data metrics from volatile ones
data Slope = Zero | Positive | Negative | Both | Unspecified
      deriving (Data, Typeable, Show, Eq, Enum)

-- | Metric types supported by Ganglia
data MetricType = String | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Float | Double
      deriving (Data, Typeable, Eq, Show)

-- | Concrete metric type used to emit metadata and value packets
data Metric = Metric
    { name  :: BS.ByteString
    , type' :: MetricType
    , units :: BS.ByteString
    , value :: BS.ByteString
    , host  :: BS.ByteString
    , spoof :: BS.ByteString
    , group :: BS.ByteString
    , slope :: Slope
    , tmax  :: Word32
    , dmax  :: Word32
    } deriving (Show)

instance Default Metric where
    def = defaultMetric

--
-- API
--

-- | A default metric record
defaultMetric :: Metric
defaultMetric = Metric
    { name  = "magical_metric"
    , type' = UInt16
    , units = ""
    , value = "0"
    , host  = ""
    , spoof = ""
    , group = ""
    , slope = Both
    , tmax  = 60
    , dmax  = 0
    }

-- | Create a new unconnected socket handle for UDP communication
open :: String -> String -> IO I.Handle
open = I.open Datagram

-- | Emit a metric's metadata and value on the specified socket handle
emit :: Metric -> I.Handle -> IO I.Handle
emit metric handle@(I.Handle sock addr) = do
    sIsConnected sock >>= \b -> unless b $ connect sock addr
    _ <- push putMetaData handle
    _ <- push putMetric handle
    return handle
  where
    push fn = I.emit . runPut $ fn metric

--
-- Private
--

-- TODO: enforce max buffer size length checks.
-- Magic number is per libgmond.c
bufferSize :: Integer
bufferSize = 1500

--
-- Binary Encoding
--

-- | Metric metadata
--
-- The format for this can be found in either:
-- * gm_protocol.x in the Ganglia 3.1 sources
-- * https://github.com/lookfirst/jmxtrans
putMetaData :: Metric -> Put
putMetaData metric@Metric{..} = do
    putHeader 128 metric -- 128 = metadata_msg
    putType type'
    putString name
    putString units
    putEnum slope
    putUInt tmax
    putUInt dmax
    putGroup group

-- | Metric value
putMetric :: Metric -> Put
putMetric metric@Metric{..} = do
    putHeader 133 metric -- 133 = string_msg
    putString "%s"
    putString value

-- | Common headers for the metadata and value
putHeader :: Int32 -> Metric -> Put
putHeader code Metric{..} = do
    putInt code
    putString host
    putString name
    putString spoof

-- | Encode either a end of message delimiter or
-- an extra group field (Ganglia 3.1 only)
putGroup :: BS.ByteString -> Put
putGroup group | BS.null group = putInt 0
               | otherwise     = do
                     putInt 1
                     putString "GROUP"
                     putString group

putInt :: Int32 -> Put
putInt = putWord32be . fromIntegral

putUInt :: Word32 -> Put
putUInt = putWord32be

putEnum :: Enum a => a -> Put
putEnum = putInt . fromIntegral . fromEnum

putString :: BS.ByteString -> Put
putString bstr = do
    putInt $ fromIntegral len
    putByteString bstr
    case fromIntegral len .&. 3 of
        0 -> return ()
        m -> putByteString $ B.replicate (4 - m) 0
  where
    len = BS.length bstr

putType :: MetricType -> Put
putType = putString . BS.pack . map toLower . show

--
-- Development
--

-- | Exactly one second!
oneSecond :: Int
oneSecond = 1000000

-- | The min, max variance of test metrics
variance :: (Int, Int)
variance = (0, 1000)

-- | Sample a random element from a list
sample :: [a] -> IO a
sample xs = liftM (xs !!) (randomRIO (0, length xs - 1))

-- | Used to emit a default metric via GHCi
test :: IO a
test = do
    conn <- open "239.2.11.71" "8649"
    loop conn
  where
    loop h = do
        r <- randomRIO variance :: IO Int
        n <- sample ["magic", "candy", "unicorns"]
        f <- emit defaultMetric { name = n, value = BS.pack $ show r } h
        threadDelay oneSecond
        loop f
