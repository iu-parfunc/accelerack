#!/bin/bash

set -xe

stack --version
stack test --no-terminal 
mkdir -p ../build
stack --local-bin-path ../build install --no-terminal
