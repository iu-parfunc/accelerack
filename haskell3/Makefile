clean:
	rm *.hi *.o *_stub.h *.so *~ *.exe
so:
	ghc --make Example.hs -shared -dynamic -fPIC -o libexample.so -lHSrts-ghc7.8.3
st:
	ghc -dynamic -fPIC -c Example.hs
st2:
	ghc -shared Example.o -o libexample.so
compile:
	ghc --make init.c -no-hs-main -lexample -o libinit.so -shared -fPIC -dynamic -L.
test: so compile
	racket ffi-test2.rkt
