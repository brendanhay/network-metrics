-- |
-- Module      : Network.Metric
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Network.Metric (
    -- * Exported Types
      SinkType(..)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    , Measurable(..)
    , Encodable(..)
    , Sink(..)
    , AnyMeasurable(..)
    , AnySink(..)
    , open
    ) where

import Data.Data               (Data, Typeable)
import Network.Metric.Internal

import qualified Network.Metric.Sink.Ganglia  as GangliaSink
import qualified Network.Metric.Sink.Graphite as GraphiteSink
import qualified Network.Metric.Sink.Statsd   as StatsdSink
import qualified Network.Metric.Sink.Handle   as H

-- | An enumeration of supplied sink types
data SinkType =
      Ganglia
    | Graphite
    | Statsd
    | Stdout
      deriving (Eq, Data, Typeable, Read, Show)

--
-- API
--

-- | Open a new sink specified by SinkType
open :: SinkType -> Host -> HostName -> PortNumber -> IO AnySink
open Ganglia  = GangliaSink.open
open Graphite = GraphiteSink.open
open Statsd   = StatsdSink.open
open Stdout   = \h _ _ -> return . AnySink $ H.SinkHandle h putStrLn
