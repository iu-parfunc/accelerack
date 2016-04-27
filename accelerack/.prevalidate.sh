#!/bin/bash

set -xe

# The version to use for testing.
export RACKET_VERSION=6.4
export RACKET_DIR="$HOME/racket_for_accelerack"
RACKETBIN="$RACKET_DIR/bin"

function get_racket() {
    TOP=`pwd`
    TEMPDIR=`tempfile`_dir
    mkdir -p $TEMPDIR
    cd $TEMPDIR
    # Uses RACKET_DIR and RACKET_VERSION:
    bash < $TOP/.install_racket.sh
}

function ver_check() {
    if [ "$VER" == "Welcome to Racket v"$RACKET_VERSION"." ];
    then echo "Racket version $RACKET_VERSION found.  Good."
    else echo "Racket found, but it reports version:"
         echo "   $VER"
         echo "So I'm reinstalling it."
         get_racket
    fi
}

if [ -e "$RACKETBIN/racket" ]; then
    RACKET=$RACKETBIN/racket   
    VER=`$RACKETBIN/racket -v`
    ver_check
elif which racket; then
    VER=`racket -v`
    ver_check
else
    echo "Racket not found, installing."
    get_racket    
fi

# If we installed it, otherwise it's already in path:
if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi

# This causes annoying problems:
find . -name ".#*" | xargs rm -f

racket --version
raco pkg install --skip-installed -u --auto c-defs
raco pkg update --skip-uninstalled --link ../accelerack
raco pkg install --skip-installed --link ../accelerack
#make clean
#make
