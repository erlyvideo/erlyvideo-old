include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/erlyvideo-$(VERSION)
DESTROOT:=$(CURDIR)/debian/erlyvideo
ERL_LIBS:=apps:deps:plugins



# NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
# 
# ifeq ($(shell uname), Linux)
# NIF_FLAGS := gcc -shared -O3 -fPIC -fno-common -Wall
# endif
# 
# ifeq ($(shell uname), Darwin)
# NIF_FLAGS := cc -arch i386 -arch x86_64 -pipe -bundle -undefined dynamic_lookup -O3 -fPIC -fno-common -Wall
# endif
# 
# ifeq ($(shell uname), FreeBSD)
# NIF_FLAGS := cc -shared -O3 -fPIC -fno-common -Wall
# endif

ERL=erl +A 4 +K true
APP_NAME=ems

all: compile

update:
	git pull

compile:
	./rebar compile

release: compile
	./rebar generate force=1
	chmod +x erlyvideo/bin/erlyvideo

ebin/mmap.so: src/core/mmap.c
	$(NIF_FLAGS) -o $@ $< -I $(NIFDIR) || touch $@

archive:
	git archive --prefix=erlyvideo-$(VERSION)/ v$(VERSION) | gzip -9 > ../erlyvideo-$(VERSION).tar.gz

tgz: release
	tar zcvf erlyvideo-$(VERSION).tar.gz erlyvideo


clean:
	./rebar clean
	rm -fv plugins/*/ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css


run: priv/erlyvideo.conf priv/log4erl.conf compile
	ERL_LIBS=apps:.. erl -boot start_sasl -s erlyvideo

priv/log4erl.conf: priv/log4erl.conf.sample
	[ -f priv/log4erl.conf ] || cp priv/log4erl.conf.sample priv/log4erl.conf

priv/erlyvideo.conf: priv/erlyvideo.conf.sample
	[ -f priv/erlyvideo.conf ] || cp priv/erlyvideo.conf.sample priv/erlyvideo.conf


deb: release
	fpm -s dir -t deb -n erlyvideo -v 2.8.2 -a x86_64 -m "Max Lapshin <max@maxidoors.ru>" --prefix /opt erlyvideo

.PHONY: doc debian compile

