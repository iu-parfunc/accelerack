

.PHONY: so clean test

so:
	gcc -shared -fPIC -o libacc.so acc_cfile.c

test: so
	racket acc_header.rkt
	racket ArrayUtils.rkt
	racket accelerack.rkt
	racket allocate.rkt
	racket verify-accelerack.rkt
clean:
	rm -f *.so
