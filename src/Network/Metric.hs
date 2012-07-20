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

    -- * Sink Functions
    , open
    , Sink(push, close)

    -- * Re-exports
    , Group
    , Bucket
    , Metric(..)
    , MetricSink(..)
    ) where

import Data.Data                (Data, Typeable)
import Network.Metric.Internal

import qualified Network.Metric.Ganglia  as GA
import qualified Network.Metric.Graphite as GR
import qualified Network.Metric.Statsd   as S

-- | An enumeration of supplied sink types
data SinkType =
      Ganglia
    | Graphite
    | Statsd
    | Stdout
      deriving (Data, Typeable, Show)

-- | A handle to a stdout sink
data StdoutSink = StdoutSink deriving (Show)

instance Sink StdoutSink where
    push  _ = print
    close _ = return ()

--
-- API
--

-- | Open a new sink specified by SinkType
open :: SinkType -> String -> String -> IO MetricSink
open Ganglia  = GA.open
open Graphite = GR.open
open Statsd   = S.open
open Stdout   = \_ _ -> return $ MetricSink StdoutSink
