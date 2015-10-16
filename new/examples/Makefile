so:
	gcc -shared -fPIC -o libacclib.so acclib.c
test: so
	racket accelerack.rkt
	racket allocate.rkt
	racket verify-accelerack.rkt
clean:
	rm *.so
