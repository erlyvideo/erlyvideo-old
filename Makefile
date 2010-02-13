VERSION=1.2
RTMPDIR=`./root`/lib/rtmp-$(VERSION)
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
DOCDIR=$(RTMPDIR)/doc/
INCLUDEDIR=$(RTMPDIR)/include/
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-rtmp

all: doc
	erl -make
	
doc:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application   "'rtmp'" '"."' '[{def,{vsn,"$(VERSION)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

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
	debuild -us -uc
	cp ../erlang-rtmp_$(VERSION)*.deb $(DEBIANREPO)/binary/
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)

deploy-doc:
	(cd doc; rsync -avz . -e ssh erlyvideo.org:/apps/erlyvideo/www/public/rtmp)

.PHONY: doc debian
