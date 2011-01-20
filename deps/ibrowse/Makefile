IBROWSE_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/ibrowse.app.src)
ERLDIR=$(ERLANG_ROOT)/lib/erlydtl-$(IBROWSE_VSN)

all:
	./rebar compile

clean:
	./rebar clean

install: all
	mkdir -p $(DESTDIR)/$(ERLDIR)
	cp -r ebin $(DESTDIR)/$(ERLDIR)/

test: all
	(cd test; make) 
	erl -noshell -pa ebin -pa test -s ibrowse -s ibrowse_test unit_tests \
	-s ibrowse_test verify_chunked_streaming \
	-s ibrowse_test test_chunked_streaming_once \
	-s erlang halt

