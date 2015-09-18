compile:
	rebar3 compile

install:
	make compile
	make doinstall

doinstall:
	erl -pa _build/default/lib/Ranker/ebin/ -noshell -run ranker install -run erlang halt
