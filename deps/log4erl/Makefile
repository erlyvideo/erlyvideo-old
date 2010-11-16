ERLDIR=$(ERLANG_ROOT)/lib/log4erl-$(VERSION)

SRC = src

all:    subdirs

subdirs: 
	cd ${SRC}; $(MAKE)

# remove all the code
clean: 
	rm -rf ebin/*.beam erl_crash.dump
	rm -f *~
	rm -f src/*~
	rm -f ebin/*~
	rm -f include/*~
	cd ${SRC}; $(MAKE) clean
#install:
#        cp -f ebin/* ../../www/ebin

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src
	mkdir -p $(DESTROOT)$(ERLDIR)/include
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/
	install -c -m 644 src/* $(DESTROOT)$(ERLDIR)/src/
	install -c -m 644 include/* $(DESTROOT)$(ERLDIR)/include/


.PHONY: doc

