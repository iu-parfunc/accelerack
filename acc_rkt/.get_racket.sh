#!/bin/bash

set -xe

if [[ -z "$RACKET_VERSION" ]]; then
    echo "Racket version environment variable not set, setting default"
    export RACKET_VERSION=6.3  # set default Racket version
    echo "Version: $RACKET_VERSION"
fi

if [[ -z "$RACKET_DIR" ]]; then
    echo "Racket directory environment variable not set, setting default"
    export RACKET_DIR="$HOME/racket"
    echo "Directory: $RACKET_DIR"
fi

TOP=`pwd`
TEMPDIR=`tempfile`_dir
mkdir -p $TEMPDIR
cd $TEMPDIR
bash < $TOP/.install_racket.sh

# which racket &>/dev/null
# ESTATUS=$?
# if [[ -n "$ESTATUS" ]]; then
#     echo "Adding racket to PATH"
#     export PATH="${PATH}:${RACKET_DIR}/bin"
# fi

# alias racket='$RACKET_DIR/bin/racket'
