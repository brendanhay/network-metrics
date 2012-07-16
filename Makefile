CABAL=`which cabal-dev`

#
# Targets
#

.PHONY: build install conf clean

all: build

build:
	$(CABAL) build

install:
	$(CABAL) install

conf:
	$(CABAL) configure

clean:
	$(CABAL) clean
