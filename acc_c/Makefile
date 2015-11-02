all: rts c

# GHC=`stack exec ghc --`
# GHC=~/.stack/programs/x86_64-osx/ghc-7.10.2/bin/ghc
GHC=~/.stack/programs/x86_64-linux/ghc-7.10.2/bin/ghc
GCC=gcc

rts:
	$(GHC) --make rts.c -o librts.so -shared -dynamic -fPIC -lHSrts-ghc7.10.2
c:
	$(GCC) -shared -fPIC -o libacc.so acc_cfile.c

clean:
	rm -f *.o *.so
