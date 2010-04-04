VERSION=`head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]'`
REQUIRED_ERLANG=R13
ERLANG_VERSION=`erl -eval 'io:format("~s", [erlang:system_info(otp_release)])' -s init stop -noshell`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/erlyvideo-$(VERSION)
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlyvideo
ERL_LIBS=deps:lib:plugins

ERL=erl +A 4 +K true
APP_NAME=ems
NODE_NAME=$(APP_NAME)@`hostname`

all: compile

compile:
	ERL_LIBS=$(ERL_LIBS) erl -make
	for dep in deps/*/ ; do (cd $$dep; echo $$dep; test -f Makefile && make -f Makefile) ; done
	@# for plugin in plugins/* ; do ERL_LIBS=../../lib:../../deps make -C $$plugin; done



erlang_version:
	@[ "$(ERLANG_VERSION)" '<' "$(REQUIRED_ERLANG)" ] && (echo "You are using too old erlang: $(ERLANG_VERSION), upgrade to $(REQUIRED_ERLANG)"; exit 1) || true

ebin:
	mkdir ebin

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


run: erlang_version priv/erlyvideo.conf
	contrib/erlyctl run

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
	cp contrib/erlyctl $(DESTROOT)/usr/bin/erlyctl
	ln -s $(DESTROOT)/usr/bin/erlyctl $(DESTROOT)/etc/init.d/erlyvideo
	cp -r wwwroot $(DESTROOT)/var/lib/erlyvideo/
	cp priv/erlyvideo.conf.debian $(DESTROOT)/etc/erlyvideo/erlyvideo.conf


debian: all
	dpkg-buildpackage
	cp ../erlyvideo_$(VERSION)_*.deb ../erlyvideo_$(VERSION).dsc $(DEBIANREPO)/binary/
	rm ../erlyvideo_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9 > binary/Packages.gz)


.PHONY: doc debian compile

