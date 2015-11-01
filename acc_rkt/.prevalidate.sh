#!/bin/bash

set -xe

RACKETBIN="$HOME/racket/bin"
if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi

if which racket; then
  echo "Racket found."
else
  ./.get_racket.sh
fi

if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi

racket --version
raco pkg install --link accelerack
#make clean
#make
