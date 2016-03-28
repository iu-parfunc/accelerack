

# Makefile common bits.

STACK=stack
CABAL=cabal

GHC= $(STACK) exec ghc --
GHC_VERSION=$(shell $(GHC) --numeric-version)

ifeq ($(shell uname),Darwin)
  OS=osx
else
  OS=linux
endif

STKROOT=$(shell $(STACK) path --global-stack-root)
