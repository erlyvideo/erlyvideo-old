VERSION:=$(shell head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]')
REQUIRED_ERLANG:=R13
ERLANG_VERSION:=$(shell erl -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop -noshell)
ERLDIR:=$(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)/lib/erlyvideo-$(VERSION)
DEBIANREPO:=/apps/erlyvideo/debian/public
DESTROOT:=$(CURDIR)/debian/erlyvideo
ERL_LIBS:=deps:lib:plugins:..

ERL=erl +A 4 +K true
APP_NAME=ems

all: compile doc

compile:
	ERL_LIBS=$(ERL_LIBS) erl -make
	[ -d deps/rtmp ] && for dep in deps/*/ ; do (cd $$dep; echo $$dep; test -f Makefile && $(MAKE) -f Makefile) ; done; true
	@# for plugin in plugins/* ; do ERL_LIBS=../../lib:../../deps $(MAKE) -C $$plugin; done

doc:
	mkdir -p doc/html
	cp -f doc/*.png doc/html/
	erl -pa ebin -s erlyvideo edoc -s init stop -noinput -noshell

erlang_version:
	@[ "$(ERLANG_VERSION)" '<' "$(REQUIRED_ERLANG)" ] && (echo "You are using too old erlang: $(ERLANG_VERSION), upgrade to $(REQUIRED_ERLANG)"; exit 1) || true


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


run: erlang_version priv/erlyvideo.conf priv/log4erl.conf
	contrib/erlyctl run

priv/log4erl.conf: priv/log4erl.conf.sample
	[ -f priv/log4erl.conf ] || cp priv/log4erl.conf.sample priv/log4erl.conf

priv/erlyvideo.conf: priv/erlyvideo.conf.sample
	[ -f priv/erlyvideo.conf ] || cp priv/erlyvideo.conf.sample priv/erlyvideo.conf
	
start: erlang_version priv/erlyvideo.conf
	contrib/erlyctl start

install: compile
	mkdir -p $(DESTROOT)/var/lib/erlyvideo/movies
	mkdir -p $(DESTROOT)$(ERLDIR)
	cp -r ebin src include lib Emakefile $(DESTROOT)$(ERLDIR)/
	mkdir -p $(DESTROOT)/usr/bin/
	cp contrib/reverse_mpegts $(DESTROOT)/usr/bin/reverse_mpegts
	cp contrib/erlyctl.debian $(DESTROOT)/usr/bin/erlyctl
	mkdir -p $(DESTROOT)/etc/init.d/
	ln -s /usr/bin/erlyctl $(DESTROOT)/etc/init.d/erlyvideo
	cp -r wwwroot $(DESTROOT)/var/lib/erlyvideo/
	cp priv/erlyvideo.conf.debian $(DESTROOT)/etc/erlyvideo/erlyvideo.conf
	cp priv/log4erl.conf.debian $(DESTROOT)/etc/erlyvideo/log4erl.conf
	cp priv/production.config $(DESTROOT)/etc/erlyvideo/production.config


debian: all
	dpkg-buildpackage -rfakeroot -D -i -I.git -Icontrib/ErlyVideo -Iwwwroot/player/.git -Imovies -Ideps -Imnesia-data -Iplugins -Ilog -S -sa
	dput erly ../erlyvideo_$(VERSION)_source.changes
	(debuild -us -uc; cp ../erlyvideo_$(VERSION)*.deb  $(DEBIANREPO)/binary/; true)
	rm ../erlyvideo_$(VERSION)*
	(cd $(DEBIANREPO); ../update)


.PHONY: doc debian compile

