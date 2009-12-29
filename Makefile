VERSION=0.9
RTMPDIR=/usr/lib/erlyvideo
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
DOCDIR=$(RTMPDIR)/doc/
INCLUDEDIR=$(RTMPDIR)/include/
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlyvideo

ERL=erl +A 4 +K true
APP_NAME=ems
NODE_NAME=$(APP_NAME)@`hostname`
VSN=0.1
MNESIA_DATA=mnesia-data
MXMLC=mxmlc

all: ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib erl -make

ebin:
	mkdir ebin

ebin/erlmedia.app:
	cp priv/erlmedia.app ebin/erlmedia.app

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv deps/*/ebin/*.beam
	rm -fv lib/*/ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

player:
	$(MXMLC) -default-background-color=#000000 -default-frame-rate=24 -default-size 960 550 -optimize=true -output=wwwroot/player/player.swf wwwroot/player/player.mxml

run: ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib $(ERL) +bin_opt_info +debug \
	-pa ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME)
	
start: ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib $(ERL) -pa `pwd`/ebin \
	-sasl sasl_error_logger '{file, "sasl.log"}' \
  -kernel error_logger '{file, "erlang.log"}' \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-detached

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

