all: jake

.PHONY: jake

compiled = $(dir $(1))compiled/$(basename $(notdir $(1)))_rkt.zo

JAKEFILE=Makefile.rkt
JAKE=jake/main.rkt

$(call compiled,${JAKE}): ${JAKE} $(shell which racket)
	raco make $<

$(call compiled,${JAKEFILE}): ${JAKEFILE} $(call compiled,${JAKE}) $(shell which racket)
	raco make $<

jake: $(call compiled,${JAKEFILE})
	racket -t ${JAKEFILE} --
