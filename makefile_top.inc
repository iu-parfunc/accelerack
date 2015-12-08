

# Makefile common bits.

STACK=stack
GHC= $(STACK) exec ghc --
GHC_VERSION=$(shell $(GHC) --numeric-version)
OS=$(shell if [ `uname` == "Darwin" ]; then echo osx; else echo linux; fi)

STKROOT=$(shell $(STACK) path --global-stack-root)
RAWGHC=$(STKROOT)/programs/x86_64-$(OS)/ghc-$(GHC_VERSION)/bin/ghc
