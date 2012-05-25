all: compile test

compile:
	find . -name '*.rkt' -print0 | xargs -0 raco make

test:
	raco test -x .
