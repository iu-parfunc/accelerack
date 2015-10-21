#!/bin/bash

set -xe

which -a racket
racket --version

make clean
make

