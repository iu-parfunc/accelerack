
include ../makefile_top.inc

GCC=gcc

all: rts c

rts:
	$(RAWGHC) --make rts.c -o librts.so -shared -dynamic -fPIC -lHSrts-ghc$(GHC_VERSION)
c:
	$(GCC) -shared -fPIC -o libacc.so acc_cfile.c

clean:
	rm -f *.o *.so
