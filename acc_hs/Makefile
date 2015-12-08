
# GHC=ghc
GHC= stack exec ghc --
GHC_VERSION=$(shell $(GHC) --numeric-version)

so:
	stack build
# Hack / temp: should be installed via Cabal:
	cabal install either-unwrap MissingH split hint accelerate-0.15.1.0
	ghc --make ExInterface.hs -o librachs.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)
	ghc --make SxpInterface.hs -o libsxp.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)

clean:
	rm -f librachs.so libsxp.so
	rm -f *.o *.h *.hi
