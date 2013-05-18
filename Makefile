all: jake

.PHONY: jake

JAKEFILE=Makefile.rkt
APSE=tools/apse/main.rkt

apse:
	raco make ${APSE}
	racket -t ${APSE} --

jake:
	raco make ${JAKEFILE}
	racket -t ${JAKEFILE} --

FILES := $(shell find . -name '*.rkt')
test: ${FILES}
	@echo ${FILES} | xargs raco make
	raco test -xt . | tee test
