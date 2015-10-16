
all: test

so:
	ghc --make Example.hs -o libexample.so -shared -dynamic -fPIC -lHSrts-ghc7.10.2

so-rts:
	ghc --make rts.c -o librts.so          -shared -dynamic -fPIC -lHSrts-ghc7.10.2

test: so so-rts
	racket ffi-test.rkt
ctest: clean test

clean:
	rm *.hi *.o *_stub.h *.so
