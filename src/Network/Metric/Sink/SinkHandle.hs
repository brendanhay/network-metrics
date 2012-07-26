-- |
-- Module      : Network.Metric.Sink.SinkHandle
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric.Sink.SinkHandle (
    -- * Exported Types
      SinkHandle(..)

    -- * Sink Functions
    , open
    , Sink(..)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    ) where

import Network.Metric.Internal

import qualified Data.ByteString.Char8 as BS

-- | A generic sink handle
data SinkHandle = SinkHandle Host (String -> IO ())

instance Sink SinkHandle where
    push h = mapM_ enc . measure
      where
        enc (Counter g b v) = put "Counter" h g b v
        enc (Timer g b v)   = put "Timer" h g b v
        enc (Gauge g b v)   = put "Gauge" h g b v

    close _ = return ()

--
-- API
--

-- | Open a new sink to stdout, conforming to the interface in Metric.hs
-- .
-- If you want to create a sink to a given handle fn, use the exported SinkHandle constructor
open :: Host -> HostName -> PortNumber -> IO AnySink
open host _ _ = return . AnySink $ SinkHandle host putStrLn

--
-- Private
--

put :: Encodable a
    => BS.ByteString
    -> SinkHandle
    -> Group
    -> Bucket
    -> a
    -> IO ()
put p (SinkHandle h f) g b v = f msg
  where
    msg = BS.unpack $ BS.concat [p, ": ", key h g b, " ", encode v]
