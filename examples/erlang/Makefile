all: ebin ebin/corr.beam ebin/test.beam
	(cd imp1; make)
	(cd imp2; make)
	(cd imp3; make)
	(cd imp4; make)
	(cd imp5; make)
	(cd imp6; make)
	(cd imp7; make)
	(cd imp8; make)
	(cd imp9; make)
	(cd imp10; make)

ebin:
	mkdir -p ebin

ebin/test.beam: src/test.erl
	erlc +debug_info -I../../include -o ebin src/test.erl

ebin/corr.beam: src/corr.erl
	erlc +debug_info -I../../include -o ebin src/corr.erl

dialyzer: 
	dialyzer -c ebin ../../_build/default/lib/ranker/ebin
