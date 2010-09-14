include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
ERLDIR=$(ERLANG_ROOT)/lib/mpegts-$(VERSION)

NIFDIR := `erl -eval 'io:format("~s", [code:lib_dir(erts,include)])' -s init stop -noshell| sed s'/erlang\/lib\//erlang\//'`
NIF_FLAGS := `ruby -rrbconfig -e 'puts Config::CONFIG["LDSHARED"]'` -O3 -fPIC -fno-common -Wall


DESTROOT=$(CURDIR)/debian/erlang-mpegts




all: compile

compile: ebin/mpeg2_crc32.so ebin/mpegts_reader.so
	erl -make
	
ebin/mpeg2_crc32.so: src/mpeg2_crc32.c
	$(NIF_FLAGS)  -o $@ $< -I $(NIFDIR) || touch $@

ebin/mpegts_reader.so: src/mpegts_reader.c
	$(NIF_FLAGS)  -o $@ $< -I $(NIFDIR) || touch $@


clean:
	rm -fv ebin/*.beam ebin/*.so
	rm -f erl_crash.dump


install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.so $(DESTROOT)$(ERLDIR)/ebin/

test:
	ERL_LIBS=.. erl -pa ebin -s mpegts_reader test -s init stop -noinput -noshell


.PHONY: debian

