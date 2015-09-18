compile:
	rebar3 compile

install:
	make compile
	make doinstall

doinstall:
	erl -pa _build/default/lib/ranker/ebin/ -noshell -run ranker_install install -run erlang halt
