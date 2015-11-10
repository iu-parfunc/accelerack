#!/bin/bash

set -xe

stack setup --no-terminal 
stack test --only-snapshot --no-terminal 
cabal install accelerate --enable-shared
cabal install accelerate-io --enable-shared
make clean
make
