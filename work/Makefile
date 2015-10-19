so:
	ghc --make Capi.hs -o libcapi.so -shared -dynamic -fPIC -lHSrts-ghc7.6.3
test: so
	racket ffi-test.rkt
ctest: clean test
clean:
	rm *.hi *.o *_stub.h libcapi.so
