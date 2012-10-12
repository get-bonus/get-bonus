all: jake

.PHONY: jake

JAKEFILE=Makefile.rkt

jake:
	raco make ${JAKEFILE}
	racket -t ${JAKEFILE} --
