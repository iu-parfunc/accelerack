

include ../makefile_top.inc

so:
	echo "Using GHC version $(GHC_VERSION)"
	stack build
	echo "Performing horrible, global cabal-all install for hint:"
	cabal install either-unwrap MissingH split hint accelerate-0.15.1.0 accelerate-io-0.15.1.0 --force-reinstalls --with-ghc=$(RAWGHC)
	echo "TEMP: manually installing bridge files...  should be installed via Cabal:"
	$(RAWGHC) --make ExInterface.hs -o librachs.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)
	$(RAWGHC) --make SxpInterface.hs -o libsxp.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)

clean:
	rm -f librachs.so libsxp.so
	rm -f *.o *.h *.hi
