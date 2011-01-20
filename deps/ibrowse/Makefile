IBROWSE_VSN = $(shell sed -n 's/.*{vsn,.*"\(.*\)"}.*/\1/p' src/ibrowse.app.src)
ERLDIR=$(ERLANG_ROOT)/lib/ibrowse-$(IBROWSE_VSN)

all:
	./rebar compile

clean:
	./rebar clean

install: all
	mkdir -p $(DESTROOT)/$(ERLDIR)/ebin
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin

test: all
	(cd test; make) 
	erl -noshell -pa ebin -pa test -s ibrowse -s ibrowse_test unit_tests \
	-s ibrowse_test verify_chunked_streaming \
	-s ibrowse_test test_chunked_streaming_once \
	-s erlang halt

