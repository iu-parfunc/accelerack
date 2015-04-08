default: test examples
examples: lib/*.rkt example_apps/*.rkt
	racket example_apps/hello_world.rkt
	racket example_apps/trial.rkt
test: lib/*.rkt test/*.rkt
	racket test/types-hi-t.rkt
	racket test/types-t.rkt
	racket test/accelerack-t.rkt
