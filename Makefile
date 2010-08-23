VERSION=`head -1 debian/changelog | sed -e 's/.*\(([^)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/erlmedia-$(VERSION)

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlmedia

all:
	erl -make

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	@#install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/

debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	dput erly ../erlmedia_$(VERSION)_source.changes
	debuild -us -uc
	cp ../erlmedia_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlmedia_$(VERSION)*
	(cd $(DEBIANREPO)/..; ./update)
	

.PHONY: debian

