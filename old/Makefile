default: tests examples
examples: lib/*.rkt example_apps/*.rkt
	#racket example_apps/hello_world.rkt
	racket example_apps/trial.rkt
	racket example_apps/mandelbrot.rkt
tests: lib/*.rkt test/*.rkt
	racket test/types-hi-t.rkt
	racket test/types-t.rkt
	racket test/accelerack-t.rkt
