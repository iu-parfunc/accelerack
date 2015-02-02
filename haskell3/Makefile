clean:
	rm *.hi *.o *_stub.h *.so
so:
	ghc --make Example.hs -shared -dynamic -fPIC -o libexample.so -lHSrts-ghc7.8.3
compile:
	ghc --make rts.c -no-hs-main -lexample -o librts.so -shared -fPIC -dynamic -L.
test: so compile
	racket ffi-test.rkt
ctest: clean test
