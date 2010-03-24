VERSION=`head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/mpegts-$(VERSION)

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-mpegts


NIF_FLAGS = `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall


all: compile

compile: ebin/mpeg2_crc32.so ebin/mpegts_reader.so
	erl -make
	
ebin/mpeg2_crc32.so: src/mpeg2_crc32.c
	$(NIF_FLAGS)  -o $@ $< -I $(ERLDIR)/usr/include/ || touch $@

ebin/mpegts_reader.so: src/mpegts_reader.c
	$(NIF_FLAGS)  -o $@ $< -I $(ERLDIR)/usr/include/ || touch $@


clean:
	rm -fv ebin/*.beam ebin/*.so
	rm -f erl_crash.dump


install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.so $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/

debian:
	debuild -us -uc
	cp ../erlang-mpegts_$(VERSION)*.deb $(DEBIANREPO)/binary/
	rm ../erlang-mpegts_$(VERSION)*
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)
	

.PHONY: debian

