#!/bin/bash

# This is the script to run locally, before checkin.
# It's also the script used by Jenkins CI.

set -xe

which -a stack

# Make sure we have prereqs before going any further:
stack setup

TOP=`pwd`

cd $TOP/acc_c/
./validate.sh

cd $TOP/acc_hs/
./validate.sh

cd $TOP/accelerack/
./validate.sh
