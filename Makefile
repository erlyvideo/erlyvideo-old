VERSION=`head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/rtmp-$(VERSION)
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-rtmp

all: 
	erl -make
	
analyze:
	 dialyzer -Wno_improper_lists -c src/*.erl

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
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/contrib
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 755 contrib/* $(DESTROOT)$(ERLDIR)/contrib
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include

debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	dput erly ../erlang-rtmp_$(VERSION)_source.changes
	debuild -us -uc
	cp ../erlang-rtmp_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-rtmp_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)

deploy-doc:
	(cd doc; rsync -avz . -e ssh erlyvideo.org:/apps/erlyvideo/www/public/rtmp)

.PHONY: doc debian

