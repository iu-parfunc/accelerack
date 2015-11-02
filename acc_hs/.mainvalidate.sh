#!/bin/bash

set -xe

stack test --no-terminal 
mkdir -p ../build
stack --local-bin-path=../build install --no-terminal
