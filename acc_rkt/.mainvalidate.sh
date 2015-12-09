#!/bin/bash

set -xe

# Annoying tu duplicate this:
RACKETBIN="$HOME/racket/bin"
if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi

# Just in case there are unit tests in the main source tree, we
# include it too:
raco test ./accelerack/ ./tests/
