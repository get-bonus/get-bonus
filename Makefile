all: jake

.PHONY: jake

JAKEFILE=Makefile.rkt

test-make:
	find . -name '*.rkt' -exec raco make {} \;

test-show:
	raco test -xt .

test:
	raco test -xtQq .

jake:
	raco make ${JAKEFILE}
	racket -t ${JAKEFILE} --
