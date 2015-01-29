-- Module      : Network.Metric.Sink.Handle
-- Copyright   : (c) 2012-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.Metric.Sink.Handle (
    -- * Exported Types
      SinkHandle(..)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    , Sink(..)
    ) where

import           Network.Metric.Internal

import qualified Data.ByteString.Char8   as BS

-- | A generic sink handle
data SinkHandle = SinkHandle (Maybe Host) (String -> IO ())

instance Sink SinkHandle where
    push h = mapM_ enc . measure
      where
        enc (Counter g b v) = put "Counter" h g b v
        enc (Timer g b v)   = put "Timer" h g b v
        enc (Gauge g b v)   = put "Gauge" h g b v

    close _ = return ()

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
