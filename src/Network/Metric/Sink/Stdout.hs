-- |
-- Module      : Network.Metric.Sink.Stdout
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric.Sink.Stdout (
    -- * Sink Functions
      open
    , Sink(..)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    , counter
    , timer
    , gauge
    ) where

import Network.Metric.Internal

import qualified Data.ByteString.Char8 as BS

-- | A handle to a stdout sink
data Stdout = Stdout Host deriving (Show)

instance Sink Stdout where
    push (Stdout host) = mapM_ enc . measure
      where
        enc (Counter g b v) = put "Counter" host g b v
        enc (Timer g b v)   = put "Timer" host g b v
        enc (Gauge g b v)   = put "Gauge" host g b v

    close _ = return ()

--
-- API
--

-- | Open a new Stdout sink
open :: Host -> HostName -> PortNumber -> IO AnySink
open host _ _ = return . AnySink $ Stdout host

--
-- Private
--

put :: Encodable a => BS.ByteString -> Host -> Group -> Bucket -> a -> IO ()
put prefix host group bucket value = putStrLn s
  where
    s = BS.unpack $ BS.concat [prefix, ": ", key host group bucket, " ", encode value]
