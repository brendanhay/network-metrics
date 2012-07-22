-- |
-- Module      : GMetric.Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
      main
    ) where


import Control.Monad            (liftM, when)
import Data.Binary.Put          (runPut)
import Network.Socket           (SocketType(..))
import System.Console.CmdArgs
import System.Environment       (getArgs, withArgs)
import System.Exit              (ExitCode(..), exitWith)
import Network.Metric.Internal

import qualified Data.ByteString.Char8       as BS
import qualified Network.Metric.Sink.Ganglia as G

data Options = Options
    { optHost  :: String
    , optPort  :: String
    , optName  :: String
    , optValue :: String
    , optType  :: G.GangliaType
    , optGroup :: String
    , optUnits :: String
    , optSlope :: G.Slope
    , optTMax  :: Integer
    , optDMax  :: Integer
    , optSpoof :: String
    } deriving (Data, Typeable, Show)

--
-- API
--

main :: IO ()
main = parse >>= emit

--
-- Private
--

emit :: Options -> IO ()
emit Options{..} = do
    sink@(G.Ganglia hd) <- liftM G.Ganglia (hOpen Datagram optHost optPort)
    _ <- push' G.putMetaData hd
    _ <- push' G.putValue hd
    G.close sink
  where
    push' f = flip hPush . runPut $ f metric
    metric  = G.GangliaMetric
        (BS.pack optName)
        optType
        (BS.pack optUnits)
        (BS.pack optValue)
        ""
        (BS.pack optSpoof)
        (BS.pack optGroup)
        optSlope
        (fromInteger optTMax)
        (fromInteger optDMax)

--
-- Parsing
--

programName, programVersion, programInfo, copyright :: String
programName    = "gmetric-haskell"
programVersion = "0.1.0"
programInfo    = programName ++ " version " ++ programVersion
copyright      = "(C) Brendan Hay <brendan@soundcloud.com> 2012"

parse :: IO Options
parse = do
    raw  <- getArgs
    opts <- (if null raw then withArgs ["--help"] else id) fn
    validate opts
  where
    fn = cmdArgs $ options
        &= versionArg [explicit, name "version", name "v", summary programInfo]
        &= summary (programInfo ++ ", " ++ copyright)
        &= helpArg [explicit, name "help", name "h"]
        &= program programName

validate :: Options -> IO Options
validate opts@Options{..} = do
    exitWhen (null optHost)  "--host cannot be blank"
    exitWhen (null optPort)  "--port cannot be blank"
    exitWhen (null optName)  "--name cannot be blank"
    exitWhen (null optValue) "--value cannot be blank"
    return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

--
-- Descriptions
--

options :: Options
options = Options
    { optHost = ""
        &= name "host"
        &= typ  "STRING"
        &= help "Host channel to deliver metrics to"
        &= explicit
    , optPort = ""
        &= name "port"
        &= typ  "INT"
        &= help "Port channel to deliver metrics to"
        &= explicit
    , optName = ""
        &= name "name"
        &= typ  "STRING"
        &= help "Name of the metric"
        &= explicit
    , optValue = ""
        &= name "value"
        &= typ  "STRING"
        &= help "Value of the metric"
        &= explicit
    , optType = G.String
        &= name "type"
        &= typ  "STRING"
        &= help "Either string|int8|uint8|int16|uint16|int32|uint32|float|double"
        &= explicit
    , optUnits = ""
        &= name "units"
        &= typ  "STRING"
        &= help "Unit of measure for the value e.g. Kilobytes, Celcius (default='')"
        &= explicit
    , optGroup = ""
        &= name "group"
        &= typ  "STRING"
        &= help "Group of the metric"
        &= explicit
    , optSlope = G.Both
        &= name "slope"
        &= typ  "STRING"
        &= help "Either zero|positive|negative|both (default='both')"
        &= explicit
    , optTMax = 60
        &= name "tmax"
        &= typ  "STRING"
        &= help "The maximum time in seconds between gmetric calls (default='60')"
        &= explicit
    , optDMax = 0
        &= name "dmax"
        &= typ  "STRING"
        &= help "The lifetime in seconds of this metric (default='0')"
        &= explicit
    , optSpoof = ""
        &= name "spoof"
        &= typ  "STRING"
        &= help "IP address and name of host/device (colon separated) we are spoofing (default='')"
        &= explicit
    } &= name "gmetric-haskell"
      &= help "The Ganglia Metric Client (gmetric) announces a metric on the specified host/port"
