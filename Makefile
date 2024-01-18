all: test build
.PHONY: all

test:
	cabal test
.PHONY: test

build:
	cabal build && cabal list-bin djoths
.PHONY: build

bench:
	cabal bench
.PHONY: bench

clean:
	cabal clean
.PHONY: clean
