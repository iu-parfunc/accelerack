GHC=ghc
GHC_VERSION=$(shell $(GHC) --numeric-version)

so:
	$(GHC) --make rkt_hs_interface.hs -o librachs.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)

clean:
	rm -f librachs.so
	rm -f *.o *.h *.hi
