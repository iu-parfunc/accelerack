
# GHC=ghc
GHC= stack exec ghc --
GHC_VERSION=$(shell $(GHC) --numeric-version)

so:
	stack build
	$(GHC) --make ExUtils.hs
	$(GHC) --make ExInterface.hs -o librachs.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)

clean:
	rm -f librachs.so
	rm -f *.o *.h *.hi
