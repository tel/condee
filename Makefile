
deps:
	cabal install --only-dependencies --enable-test -j4

configure: deps
	cabal configure --enable-test

build: configure
	cabal build

test: build
	cabal test

fasttest: ./dist_condee/build/tests/tests
	cabal build
	./dist_condee/build/tests/tests

clean:
	cabal clean

all: test


.PHONY: deps configure build test all
