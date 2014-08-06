FLAGS := -j --disable-documentation --disable-library-coverage

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install $(FLAGS) --only-dependencies

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init
