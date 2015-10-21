#!/bin/bash

set -xe

if which racket; then 
  echo "Racket found."
else
  ./.get-racket.sh
fi

racket --version

make clean
make

