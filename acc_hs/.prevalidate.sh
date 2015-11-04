#!/bin/bash

set -xe

stack upgrade
stack setup --no-terminal 
stack test --only-snapshot --no-terminal 
