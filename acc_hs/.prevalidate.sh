#!/bin/bash

set -xe

stack setup --no-terminal
stack test --only-snapshot --no-terminal
# cabal install accelerate-io --enable-shared
# cabal install accelerate --enable-shared
# cabal install MissingH
# cabal install either-unwrap
# cabal install split
make clean
make
