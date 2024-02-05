all: test build
.PHONY: all

test:
	cabal test
.PHONY: test

build:
	cabal build && cabal list-bin djoths
.PHONY: build

bench: build
	cabal bench
	$(shell cabal list-bin djoths) benchmark/m.dj +RTS -s >/dev/null
.PHONY: bench

clean:
	cabal clean
.PHONY: clean
