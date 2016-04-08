#!/bin/bash

set -xe

RACKETBIN="$HOME/racket/bin"
if [ -d $RACKETBIN ]; then
    export PATH=$RACKETBIN:$PATH
fi
export MINRACKETVERSION=6.3
if which racket; then
  CURRENTRACKETV=$(racket -v | egrep -o '[0-9]+.[0-9]+')
  if [ $(echo "$CURRENTRACKETV >= $MINRACKETVERSION" | bc) -eq 1 ]; then
	  echo "Racket found" ;
  else
	  ./.get_racket.sh
  fi
else
  ./.get_racket.sh
fi

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
