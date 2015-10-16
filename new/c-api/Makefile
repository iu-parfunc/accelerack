clean:
	rm *.hi *.o *_stub.h *.so
so:
	ghc --make Capi.hs -o libcapi.so -shared -dynamic -fPIC -lHSrts-ghc7.8.3
test: so
	racket ffi-test.rkt
ctest: clean test
