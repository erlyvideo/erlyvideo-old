VERSION=1.3
REQUIRED_ERLANG=R13
ERLANG_VERSION=`erl -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop -noshell`
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

all: erlang_version ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib:plugins erl -make
	# for plugin in plugins/* ; do ERL_LIBS=../../lib:../../deps make -C $$plugin; done

erlang_version:
	@[ "$(ERLANG_VERSION)" '<' "$(REQUIRED_ERLANG)" ] && (echo "You are using too old erlang: $(ERLANG_VERSION), upgrade to $(REQUIRED_ERLANG)"; exit 1) || true

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
	rm -fv plugins/*/ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

player:
	$(MXMLC) -default-background-color=#000000 -default-frame-rate=24 -default-size 960 550 -optimize=true -output=wwwroot/player/player.swf wwwroot/player/player.mxml

run: erlang_version ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib:plugins $(ERL) +bin_opt_info +debug \
	-pa ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME)
	
start: erlang_version ebin ebin/erlmedia.app
	ERL_LIBS=deps:lib:plugins $(ERL) -pa `pwd`/ebin \
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

archive: ../erlyvideo-$(VERSION).tgz
	

../erlyvideo-$(VERSION).tgz:
	(cd ..; tar zcvf erlyvideo-$(VERSION).tgz --exclude='.git*' --exclude=build --exclude='.DS_Store' --exclude='erlyvideo/plugins/*' --exclude=erlyvideo/$(MNESIA_DATA)* --exclude='erlyvideo/*/._*' erlyvideo)

debian:
	cp ../erlang-rtmp_$(VERSION)*.deb $(DEBIANREPO)/binary/
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)


.PHONY: doc debian

