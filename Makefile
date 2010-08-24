include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/ertp-$(VERSION)

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
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/
	install -c -m 644 Emakefile $(DESTROOT)$(ERLDIR)/Emakefile
	install -c -m 644 Makefile $(DESTROOT)$(ERLDIR)/Makefile
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/


doc: src/*.erl
	erl -pa ebin -s ertp edoc -s init stop -noinput -noshell


.PHONY: debian

