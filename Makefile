VERSION=`head -1 debian/changelog | sed -Ee 's/.*\(([^\)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/shoutcast-$(VERSION)

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-shoutcast

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
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/

debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	dput erly ../erlang-shoutcast_$(VERSION)_source.changes
	debuild -us -uc
	cp ../erlang-shoutcast_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-shoutcast_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)
	

.PHONY: debian

