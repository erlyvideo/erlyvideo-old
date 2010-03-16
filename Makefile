ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`
VERSION=`head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]'`

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-rtmp

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
	@#install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/

debian:
	debuild -us -uc
	cp ../erlang-shoutcast_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-shoutcast_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)
	

.PHONY: debian

