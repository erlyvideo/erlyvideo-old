ERLDIR=$(ERLANG_ROOT)/lib/rtmp-$(VERSION)

DESTROOT=$(CURDIR)/debian/erlang-rtmp

all: compile
	erl -make

compile:
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
	mkdir -p $(DESTROOT)/usr/bin
	mkdir -p $(DESTROOT)$(ERLDIR)/contrib
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 755 contrib/* $(DESTROOT)$(ERLDIR)/contrib
	install -c -m 755 contrib/rtmp_bench $(DESTROOT)/usr/bin/rtmp_bench
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include

deploy-doc:
	(cd doc; rsync -avz . -e ssh erlyvideo.org:/apps/erlyvideo/www/public/rtmp)

.PHONY: doc debian

