include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/shoutcast-$(VERSION)

DESTROOT=$(CURDIR)/debian/erlang-shoutcast

all:
	erl -make

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/


.PHONY: debian

