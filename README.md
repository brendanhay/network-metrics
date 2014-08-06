Network.Metric
==============

Table of Contents
-----------------

* [Contribute](#contribute)
* [Licence](#licence)


<a name="usage" />

Usage
-----

All modules including `Network.Metric` expose the same interfaces to sinks, and re-export
the required types for constructing metrics.

Supported Sinks:

* `Network.Metric.Sink.Ganglia`
* `Network.Metric.Sink.Graphite`
* `Network.Metric.Sink.Statsd`
* `Network.Metric.Sink.Stdout`


**Unified:**

````haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Metric

main = do
    sink <- open Ganglia (Just "thishost") "localhost" "1234"
    -- Creates ganglia key: "thishost.name.space.bucket" with an "int32" type
    push sink $ Counter "name.space" "bucket" 1234
    close sink
````


**Specific Sink:**

````haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.Metric.Sink.Graphite

main = do
    sink <- open Nothing "localhost" "1234"
    -- Creates graphite key: "name.space.bucket"
    push sink $ Counter "name.space" "bucket" 1234
    close sink
````


<a name="api" />

API
---

Preliminary API documentation is available [on Hackage](http://hackage.haskell.org/package/network-metrics).

> The API is currently in flux, and conversion between the universal `Counter`, `Gauge`, and `Timing` ctors to the respective sink types is not yet completed.

<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/network-metrics/issues).


<a name="licence" />

Licence
-------

network-metrics is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
