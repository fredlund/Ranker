EFLAGS = +debug_info
SOURCES = $(wildcard *.erl *.erl)
BEAMS = $(patsubst %.erl,../ebin/%.beam,$(SOURCES))

all: ../ebin $(BEAMS)

../ebin:
	mkdir -p ../ebin

../ebin/%.beam: %.erl
	erlc $(EFLAGS) -o ../ebin $<

