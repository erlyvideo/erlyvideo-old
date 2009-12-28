VERSION=0.9
RTMPDIR=/usr/lib/erlang/lib/rtmp-$(VERSION)
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
DOCDIR=$(RTMPDIR)/doc/
INCLUDEDIR=$(RTMPDIR)/include/

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
	mkdir -p $(DESTDIR)$(BEAMDIR)
	mkdir -p $(DESTDIR)$(DOCDIR)
	mkdir -p $(DESTDIR)$(SRCDIR)
	mkdir -p $(DESTDIR)$(INCLUDEDIR)
	install -c -m 644 ebin/*.beam $(DESTDIR)$(BEAMDIR)
	install -c -m 644 ebin/*.app $(DESTDIR)$(BEAMDIR)
	install -c -m 644 doc/* $(DESTDIR)$(DOCDIR)
	install -c -m 644 src/* $(DESTDIR)$(SRCDIR)
	install -c -m 644 include/* $(DESTDIR)$(INCLUDEDIR)

.PHONY: doc
