#!/bin/bash

set -xe

stack --version
stack test --no-terminal
cd ..
mkdir -p ./build
stack --local-bin-path ./build install --no-terminal
