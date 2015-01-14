

GHCROOT=$(HOME)/opt/ghc-7.8.3/

# For HsFFI.h
INCLUDE=$(GHCROOT)/lib/ghc-7.8.3/include/

LIBS=$(GHCROOT)/lib/ghc-7.8.3/rts-1.0/

direct:
	ghc -no-hs-main wrapper.c example.hs -o wrapper_ghc1.exe

compile:
	ghc --make -fPIC -dynamic -shared -o libexample.so -lHSrts-ghc7.8.3 -optl-Wl,-rpath,$(GHCROOT)/lib/ghc-7.8.3 example.hs
# Getting a gmp problem here:
	gcc -c wrapper.c -I$(INCLUDE) -o wrapper.o

# Multi-step compile then link but still with GHC:
ghc: compile
	ghc -no-hs-main wrapper.o libexample.so -o wrapper_ghc2.exe

# Compile then link through GCC:
# Trying to follow http://www.well-typed.com/blog/30/
gcc: compile
# Attempt to link in C... undefined symbol hs_init.  But it SHOULD be in libHSrts
#	gcc -I$(INCLUDE) $(LIBS) ./example.so wrapper.o -o wrapper.exe
#	gcc -I$(INCLUDE) -L$(LIBS) -L.  -Wl,-rpath,'$ORIGIN'  -lHSrts -lexample   wrapper.o -o wrapper.exe
	gcc -I$(INCLUDE) -lffi -L$(LIBS) -L.  -Wl,-rpath      -lHSrts -lexample   wrapper.o -o wrapper.exe

racket: compile
	LD_LIBRARY_PATH=.:$(HOME)/opt/ghc-7.8.3/lib/ghc-7.8.3/rts-1.0/:$LD_LIBRARY_PATH racket ffi-test.rkt

# Attempt to make a static library through GHC
static:
	ghc -c -o example.a example.hs
	gcc -c wrapper.c -I$(INCLUDE) -o wrapper.o
#	ghc -no-hs-main wrapper.o example.so -o wrapper_ghc.exe


