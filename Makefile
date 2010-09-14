include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/ertsp-$(VERSION)

DESTROOT=$(CURDIR)/debian/erlang-rtsp

all: compile doc

compile: src/*.erl
	erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/


doc: src/*.erl
	erl -pa ebin -s rtsp edoc -s init stop -noinput -noshell

.PHONY: doc
