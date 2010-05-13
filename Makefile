VERSION=`head -1 debian/changelog | sed -Ee 's/.*\(([^\)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/ertsp-$(VERSION)

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-rtsp

all:
	erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

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

debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	dput erly ../erlang-rtsp_$(VERSION)_source.changes
	debuild -us -uc
	cp ../erlang-rtsp_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-rtsp_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)
	

.PHONY: debian

