include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/erlyvideo-$(VERSION)
DESTROOT:=$(CURDIR)/debian/erlyvideo
ERL_LIBS:=deps:lib:plugins:..

ERL=erl +A 4 +K true
APP_NAME=ems

all: deps/amf snmp compile 

update: update_deps
	git pull

deps/amf: update_deps

update_deps: rebar.config
	[ -d wwwroot/player ] || git clone git://github.com/erlyvideo/erlyplayer wwwroot/player
	./rebar get-deps


push:
	git push github master
	git push github master --tags

rebar.config:
	cp rebar.config.sample rebar.config

compile:
	ERL_LIBS=$(ERL_LIBS) erl -make
	[ -d deps/erlydtl ] && (cd deps/erlydtl && make) || true
	(cd deps/mpegts && make)
	
include/ERLYVIDEO-MIB.hrl: snmp/ERLYVIDEO-MIB.bin
	erlc -o include snmp/ERLYVIDEO-MIB.bin

snmp/ERLYVIDEO-MIB.bin: snmp/ERLYVIDEO-MIB.mib
	erlc -o snmp snmp/ERLYVIDEO-MIB.mib


archive: ../erlyvideo-$(VERSION).tgz


../erlyvideo-$(VERSION).tgz:
	(cd ..; tar zcvf erlyvideo-$(VERSION).tgz --exclude='.git*' --exclude='.DS_Store' --exclude='erlyvideo/plugins/*' --exclude=erlyvideo/$(MNESIA_DATA)* --exclude='erlyvideo/*/._*' erlyvideo)


ebin:
	mkdir ebin

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


run: priv/erlyvideo.conf priv/log4erl.conf
	contrib/erlyctl run

priv/log4erl.conf: priv/log4erl.conf.sample
	[ -f priv/log4erl.conf ] || cp priv/log4erl.conf.sample priv/log4erl.conf

priv/erlyvideo.conf: priv/erlyvideo.conf.sample
	[ -f priv/erlyvideo.conf ] || cp priv/erlyvideo.conf.sample priv/erlyvideo.conf
	
start: priv/erlyvideo.conf
	contrib/erlyctl start

install: compile
	mkdir -p $(DESTROOT)/var/lib/erlyvideo/movies
	mkdir -p $(DESTROOT)/var/lib/erlyvideo/plugins
	mkdir -p $(DESTROOT)$(ERLDIR)
	cp -r ebin src include lib Emakefile $(DESTROOT)$(ERLDIR)/
	mkdir -p $(DESTROOT)/usr/bin/
	cp contrib/reverse_mpegts $(DESTROOT)/usr/bin/reverse_mpegts
	cp contrib/erlyctl.debian $(DESTROOT)/usr/bin/erlyctl
	mkdir -p $(DESTROOT)/etc/init.d/
	ln -s /usr/bin/erlyctl $(DESTROOT)/etc/init.d/erlyvideo
	cp -r wwwroot $(DESTROOT)/var/lib/erlyvideo/
	rm -rf $(DESTROOT)/var/lib/erlyvideo/wwwroot/player/.git
	mkdir -p $(DESTROOT)/var/log/erlyvideo
	mkdir -p $(DESTROOT)/etc/erlyvideo
	cp priv/erlyvideo.conf.debian $(DESTROOT)/etc/erlyvideo/erlyvideo.conf
	cp priv/log4erl.conf.debian $(DESTROOT)/etc/erlyvideo/log4erl.conf
	cp priv/production.config.debian $(DESTROOT)/etc/erlyvideo/production.config
	cp -r snmp $(DESTROOT)/var/lib/erlyvideo/
	mkdir -p $(DESTROOT)/var/cache/erlyvideo/licensed
	for i in deps/erlmedia deps/mpegts deps/rtmp deps/rtp deps/rtsp ; do (cd $$i; make DESTROOT=$(DESTROOT) ERLANG_ROOT=$(ERLANG_ROOT) VERSION=$(VERSION) install) ; done


.PHONY: doc debian compile snmp

