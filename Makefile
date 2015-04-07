default: test
examples: example_apps/*.rkt
	racket example_apps/hello_world.rkt
	racket example_apps/trial.rkt
test: test/*.rkt
	racket test/types-hi-t.rkt
	racket test/types-t.rkt
	racket test/accelerack-t.rkt
