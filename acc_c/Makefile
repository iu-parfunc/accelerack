all: rts c

# GHC=`stack exec ghc --`
OS=$(shell if [ `uname` == "Darwin" ]; then echo osx; else echo linux; fi)

# GHC=~/.stack/programs/x86_64-$(OS)/ghc-7.10.2/bin/ghc
GCC=gcc

rts:
	ghc --make rts.c -o librts.so -shared -dynamic -fPIC -lHSrts-ghc7.6.3
c:
	$(GCC) -shared -fPIC -o libacc.so acc_cfile.c

clean:
	rm -f *.o *.so
