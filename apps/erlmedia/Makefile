ERLDIR=$(ERLANG_ROOT)/lib/erlmedia-$(VERSION)

DESTROOT=$(CURDIR)/debian/erlmedia

all:
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


.PHONY: debian

