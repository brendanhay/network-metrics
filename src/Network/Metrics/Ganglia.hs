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
    , Ganglia(..)

    -- * Default constructors
    , defaultMetric

    -- * Socket Handle operations
    , open

    -- * Binary encoding
    , putMetaData
    , putValue

    -- * Re-exported from internal
    , I.Handle
    , I.push
    , I.close
    ) where

import Data.Binary.Put
import Data.Bits          ((.&.))
import Data.Char          (toLower)
import Data.Data          (Data, Typeable)
import Data.Default       (Default, def)
import Data.Int           (Int32)
import Data.Word          (Word32)
import Network.Socket

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Metrics.Internal   as I

-- | Allows gmetad and the PHP webfrontend to efficiently separate
-- constant data metrics from volatile ones
data Slope = Zero | Positive | Negative | Both | Unspecified
      deriving (Data, Typeable, Show, Eq, Enum)

-- | Metric types supported by Ganglia
data MetricType = String | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Float | Double
      deriving (Data, Typeable, Eq, Show)

-- | concrete metric type used to emit metadata and value packets
data Metric = Metric
    { name  :: BS.ByteString -- use
    , type' :: MetricType    -- use a default int32 type to support signed values
    , units :: BS.ByteString -- think of a simple value rep for counters, timers, and gauges
    , value :: BS.ByteString -- use
    , host  :: BS.ByteString -- leave blank
    , spoof :: BS.ByteString -- leave blank
    , group :: BS.ByteString -- this can be in the global type
    , slope :: Slope  -- just leave as default, both
    , tmax  :: Word32 -- leave as default, 60
    , dmax  :: Word32 -- the lifetime, for guages this is 0
                      -- what about for counters, timers?
    } deriving (Show)

instance Default Metric where
    def = defaultMetric

data Ganglia = Ganglia

instance I.MetricSink Ganglia where
    encode _ _ = return . BL.concat $ map f [putMetaData, putValue]
      where
        f g  = runPut $ g conv
        conv = defaultMetric -- conv m

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
putValue :: Metric -> Put
putValue metric@Metric{..} = do
    putHeader 133 metric -- 133 = string_msg
    putString "%s"
    putString value

--
-- Private
--

-- TODO: enforce max buffer size length checks.
-- Magic number is per libgmond.c
-- bufferSize :: Integer
-- bufferSize = 1500

--
-- Binary Encoding
--

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
