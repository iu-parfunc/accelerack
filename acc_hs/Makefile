

include ../makefile_top.inc

DEPS=deps

so:
	echo "Using GHC version $(GHC_VERSION)"
	$(STACK) build --ghc-options=-O0
#	echo "Performing horrible, global cabal-all install for hint:"
# $(CABAL) install $(DEPS)/sexp bytestring either-unwrap MissingH split hint accelerate-0.15.1.0 accelerate-io-0.15.1.0 --force-reinstalls --with-ghc=$(RAWGHC)
# echo "TEMP: manually installing bridge files...  should be installed via Cabal:"
# $(RAWGHC) --make DirectInterface.hs -o librachs.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)
# $(RAWGHC) --make InterpInterface.hs -o libsxp.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)

clean:
	rm -f librachs.so libsxp.so
	rm -f *.o *.h *.hi
