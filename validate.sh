#!/bin/bash

# This is the script to run locally, before checkin.
# It's also the script used by Jenkins CI.

set -xe

which -a stack

cd acc_hs/
stack setup --no-terminal 
stack test --only-snapshot --no-terminal 

stack test --no-terminal 
