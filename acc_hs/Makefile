

include ../makefile_top.inc

DEPS=deps

so:
	echo "Using GHC version $(GHC_VERSION)"
	$(STACK) build --ghc-options='-O0 -lHSrts-ghc$(GHC_VERSION)'
	$(STACK) --version
	$(STACK) test --no-terminal --only-snapshot
	mkdir -p ../build
	$(STACK) --local-bin-path ./build install --no-terminal --ghc-options='-O0 -lHSrts-ghc$(GHC_VERSION)'

clean:
	rm -f librachs.so libsxp.so
	rm -f *.o *.h *.hi
