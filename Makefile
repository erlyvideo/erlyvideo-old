VERSION=`head -1 debian/changelog | sed -Ee 's/.*\(([^\)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/log4erl-$(VERSION)
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
INCLUDEDIR=$(RTMPDIR)/include/
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/log4erl

SRC = src

all:    subdirs

subdirs: 
	cd ${SRC}; $(MAKE)

# remove all the code
clean: 
	rm -rf ebin/*.beam erl_crash.dump
	rm -f *~
	rm -f src/*~
	rm -f ebin/*~
	rm -f include/*~
	cd ${SRC}; $(MAKE) clean
#install:
#        cp -f ebin/* ../../www/ebin

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/

debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	dput erly ../log4erl_$(VERSION)_source.changes
	debuild -us -uc
	cp ../log4erl_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../log4erl_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)


.PHONY: doc debian

