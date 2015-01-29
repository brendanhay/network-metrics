SHELL := /usr/bin/env bash

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install  -j \
 --disable-documentation \
 --only-dependencies

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init
