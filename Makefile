all: jake

.PHONY: jake

JAKEFILE=Makefile.rkt

test-show:
	raco test -xt .

test:
	find . -name '*.rkt' | xargs raco make
	raco test -xtQq .

jake:
	raco make ${JAKEFILE}
	racket -t ${JAKEFILE} --
