all: rts c

rts:
	ghc --make rts.c -o librts.so -shared -dynamic -fPIC -lHSrts-ghc7.6.3
c:
	gcc -shared -fPIC -o libacc.so acc_cfile.c
clean:
	rm -f *.o *.so
