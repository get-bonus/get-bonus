all: jake

.PHONY: jake

JAKEFILE=Makefile.rkt

jake:
	raco make ${JAKEFILE}
	racket -t ${JAKEFILE} --

FILES := $(shell find . -name '*.rkt')
test: ${FILES}
	@echo ${FILES} | xargs raco make
	raco test -xt . | tee test
