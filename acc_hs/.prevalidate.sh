#!/bin/bash

set -xe

stack setup --no-terminal 
stack test --only-snapshot --no-terminal 
cabal install accelerate
make clean
make
