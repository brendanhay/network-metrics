Network.Metrics
===============

Table of Contents
-----------------

* [Usage](#usage)
* [Contribute](#contribute)
* [Licence](#licence)


<a name="usage" />

Usage
-----

````haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Metrics.Graphite

main = do
    sink <- open "localhost" "1234"
    push metric sink
    close sink
  where
    metric = Metric Counter "name.space" "bucket" "1234" -- Creates graphite key: name.space.bucket
````


<a name="contribute" />

Contribute
----------

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/network-metrics/issues).


<a name="licence" />

Licence
-------

Stetson is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
