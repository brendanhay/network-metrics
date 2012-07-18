CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build install conf clean doc package

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean

doc:
	$(CABAL) haddock

package: clean install doc
	$(CABAL) sdist

