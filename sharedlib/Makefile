clean:
	rm *.hi *.o *_stub.h *.so
so:
	ghc --make Example.hs -o libexample.so -shared -dynamic -fPIC -lHSrts-ghc7.8.3
so-rts:
	ghc --make rts.c -o librts.so          -shared -dynamic -fPIC -lHSrts-ghc7.8.3
test: so so-rts
	racket ffi-test.rkt
ctest: clean test
