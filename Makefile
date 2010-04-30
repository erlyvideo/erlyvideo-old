VERSION := $(shell head -1 debian/changelog | ruby -e 'puts STDIN.readlines.first[/\(([\d\.]+)\)/,1]')
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-rtmp

# Assume Linux-style dynamic library flags
DYNAMIC_LIB_CFLAGS = -fpic -shared
ifeq ($(shell uname),Darwin)
    DYNAMIC_LIB_CFLAGS = -arch x86_64 -fPIC -bundle -flat_namespace -undefined suppress
endif
ifeq ($(shell uname),SunOs)
    DYNAMIC_LIB_CFLAGS = -KPIC -G -z text
endif
ERL_INCLUDE_DIR := $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface,include)])' -s init stop -noshell)
ERL_LIB_DIR := $(shell erl -eval 'io:format("~s", [code:lib_dir(erl_interface,lib)])' -s init stop -noshell)
ERLDIR := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)/lib/rtmp-$(VERSION)
ERLANG_DIR := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)

OBJS := $(patsubst src/%c,ebin/%so,$(wildcard src/*.c))

all: $(OBJS)
	erl -make
	
analyze:
	 dialyzer -Wno_improper_lists -c src/*.erl

doc:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application   "'rtmp'" '"."' '[{def,{vsn,"$(VERSION)"}}]'

ebin/rtmp_codec_drv.so: src/rtmp_codec_drv.c
	gcc -g -O2 -Wall  \
	src/rtmp_codec_drv.c \
	-I$(ERL_INCLUDE_DIR) -I$(ERLANG_DIR)/usr/include \
	-L$(ERL_LIB_DIR) -lerl_interface -lei \
	-o ebin/rtmp_codec_drv.so \
	$(DYNAMIC_LIB_CFLAGS)
	


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
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 755 contrib/* $(DESTROOT)$(ERLDIR)/contrib
	install -c -m 755 contrib/rtmp_bench $(DESTROOT)/usr/bin/rtmp_bench
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.so $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src
	install -c -m 644 Makefile $(DESTROOT)$(ERLDIR)/Makefile
	install -c -m 644 Emakefile $(DESTROOT)$(ERLDIR)/Emakefile
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

