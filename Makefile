include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/erlyvideo-$(VERSION)
DESTROOT:=$(CURDIR)/debian/erlyvideo
ERL_LIBS:=deps:lib:plugins:..



NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall


ERL=erl +A 4 +K true
APP_NAME=ems

all: snmp compile

update:
	git pull

compile: ebin/mmap.so
	ERL_LIBS=$(ERL_LIBS) erl -make
	(cd deps/ibrowse && make)
	(cd deps/erlydtl && make)


include/ERLYVIDEO-MIB.hrl: snmp/ERLYVIDEO-MIB.bin
	erlc -o include snmp/ERLYVIDEO-MIB.bin

snmp/ERLYVIDEO-MIB.bin: snmp/ERLYVIDEO-MIB.mib
	erlc -o snmp snmp/ERLYVIDEO-MIB.mib

snmp: include/ERLYVIDEO-MIB.hrl


ebin/mmap.so: src/core/mmap.c
	$(NIF_FLAGS) -o $@ $< -I $(NIFDIR) || touch $@

archive:
	git archive --prefix=erlyvideo-$(VERSION)/ v$(VERSION) | gzip -9 > ../erlyvideo-$(VERSION).tar.gz

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
	mkdir -p $(DESTROOT)/var/log/erlyvideo
	mkdir -p $(DESTROOT)/etc/erlyvideo
	cp priv/erlyvideo.conf.debian $(DESTROOT)/etc/erlyvideo/erlyvideo.conf
	cp priv/log4erl.conf.debian $(DESTROOT)/etc/erlyvideo/log4erl.conf
	cp priv/production.config.debian $(DESTROOT)/etc/erlyvideo/production.config
	cp -r snmp $(DESTROOT)/var/lib/erlyvideo/
	mkdir -p $(DESTROOT)/var/cache/erlyvideo/licensed
	for i in deps/amf deps/log4erl deps/erlydtl deps/erlmedia deps/mpegts deps/rtmp deps/rtp deps/rtsp deps/ibrowse ; do (cd $$i; make DESTROOT=$(DESTROOT) ERLANG_ROOT=$(ERLANG_ROOT) VERSION=$(VERSION) install) ; done


.PHONY: doc debian compile snmp

