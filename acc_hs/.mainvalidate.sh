#!/bin/bash

set -xe

stack --version
stack test --no-terminal
mkdir -p ../build
stack install --no-terminal
