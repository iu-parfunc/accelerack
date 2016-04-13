#!/bin/bash

# This is the script to run locally, before checkin.
# It's also the script used by Jenkins CI.

set -xe

TOP=`pwd`

cd $TOP/accelerack/
./validate.sh
