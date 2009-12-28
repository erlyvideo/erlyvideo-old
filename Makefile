VERSION=0.8.6
RTMPDIR=`./root`/lib/log4erl-$(VERSION)
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
DOCDIR=$(RTMPDIR)/doc/
INCLUDEDIR=$(RTMPDIR)/include/
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/log4erl

SRC = src

all:    subdirs

subdirs: 
	cd ${SRC}; make

# remove all the code
clean: 
	rm -rf ebin/*.beam erl_crash.dump
	rm -f *~
	rm -f src/*~
	rm -f ebin/*~
	rm -f include/*~
	cd ${SRC}; make clean
#install:
#        cp -f ebin/* ../../www/ebin

install:
	mkdir -p $(DESTROOT)$(BEAMDIR)
	mkdir -p $(DESTROOT)$(DOCDIR)
	mkdir -p $(DESTROOT)$(SRCDIR)
	mkdir -p $(DESTROOT)$(INCLUDEDIR)
	install -c -m 644 ebin/*.beam $(DESTROOT)$(BEAMDIR)
	install -c -m 644 ebin/*.app $(DESTROOT)$(BEAMDIR)
	install -c -m 644 doc/* $(DESTROOT)$(DOCDIR)
	install -c -m 644 src/* $(DESTROOT)$(SRCDIR)
	install -c -m 644 include/* $(DESTROOT)$(INCLUDEDIR)

debian:
	cp ../erlang-rtmp_$(VERSION)*.deb $(DEBIANREPO)/binary/
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)


.PHONY: doc debian

