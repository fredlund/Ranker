all: ebin ebin/corr.beam ebin/test.beam
	(cd imp1; make)
	(cd imp2; make)

ebin:
	mkdir -p ebin

ebin/test.beam: src/test.erl
	erlc -o ebin src/test.erl

ebin/corr.beam: src/corr.erl
	erlc -o ebin src/corr.erl
