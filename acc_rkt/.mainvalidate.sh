#!/bin/bash

set -xe

# Annoying tu duplicate this:
RACKETBIN="$HOME/racket/bin"
if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi


racket tests.rkt
