ERLDIR=$(ERLANG_ROOT)/lib/rtp-$(VERSION)

DESTROOT=$(CURDIR)/debian/erlang-rtp

all: compile

compile: src/*.erl
	erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f *~ src/*~ include/*~

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/


doc: src/*.erl
	erl -pa ebin -s ertp edoc -s init stop -noinput -noshell


.PHONY: debian

