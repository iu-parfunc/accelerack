clean:
	rm *.hi *.o *_stub.h *.so *~
so-no-rts:
	ghc --make -dynamic -shared -fPIC Example.hs -o libexample.so
so:
	ghc --make -dynamic -shared -fPIC Example.hs -o libexample.so   -lHSrts-ghc7.8.3 -optl-Wl,-rpath,/usr/local/haskell/ghc-7.8.3-x86_64/lib/ghc-7.8.3
exe:
	ghc -no-hs-main wrapper.c Example.hs -o wrapper.exe
	#How does this work without RTS in ldd: it's statically linking
compile:
	ghc -fPIC -c init.c
so-wrapped: compile
	ghc --make -dynamic -shared -fPIC Example.hs -o libexample.so   -lHSrts-ghc7.8.3 -optl-Wl,-rpath,/usr/local/haskell/ghc-7.8.3-x86_64/lib/ghc-7.8.3
