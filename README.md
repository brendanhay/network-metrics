Network.Metrics
===============

Table of Contents
-----------------

* [Usage](#usage)
* [API](#api)
* [GMetric](#gmetric)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="usage" />

Usage
-----

Modules are intended to be import qualified if they need to be used in conjunction with each other.

Supported Sinks:

* `Network.Metrics.Ganglia`
* `Network.Metrics.Graphite`
* `Network.Metrics.Statsd`

**To use the unified sink type:**

````haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Metrics

main = do
    sink <- open Ganglia "localhost" "1234"
    push metric sink
    close sink
  where
    metric = Metric Counter "name.space" "bucket" "1234" -- Creates ganglia key: "name.space.bucket"
````

**To use a specific sink directly:**

````haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Metrics.Graphite

main = do
    sink <- open "localhost" "1234"
    push metric sink
    close sink
  where
    metric = Metric Counter "name.space" "bucket" "1234" -- Creates graphite key: "name.space.bucket"
````


<a name="api" />

API
---

Preliminary API documentation is available [on Hackage](http://hackage.haskell.org/package/network-metrics).

> The API is currently in flux, and conversion between the universal `Metric` `Counter` `Gauge` `Timing` type to the respective sink types is not completed.


<a name="gmetric" />

GMetric
-------

A port of Ganglia's `gmetric` is built by default under the name `gmetric-haskell`.


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/network-metrics/issues).


<a name="licence" />

Licence
-------

Stetson is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
