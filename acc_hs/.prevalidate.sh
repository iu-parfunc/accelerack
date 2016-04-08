#!/bin/bash

set -xe

stack setup --no-terminal
stack test --only-snapshot --no-terminal

# Temporarily disable these global installs (which were for hint):
# cabal install accelerate-io --enable-shared
# cabal install accelerate --enable-shared
# cabal install MissingH
# cabal install either-unwrap
# cabal install split
make clean

# Build the .so file for use by Racket:
make
