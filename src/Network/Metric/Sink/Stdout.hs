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
    , Sink(push, close)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    ) where

import Network.Metric.Internal

-- | A handle to a stdout sink
data StdoutSink = StdoutSink deriving (Show)

instance Sink StdoutSink where
    push  _ = print
    close _ = return ()

--
-- API
--

-- | Open a new Stdout sink
open :: String -> String -> IO MetricSink
open _ _ = return $ MetricSink StdoutSink
