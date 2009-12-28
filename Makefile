VERSION=0.5.3
RTMPDIR=`./root`/lib/erlydtl-$(VERSION)
BEAMDIR=$(RTMPDIR)/ebin/
SRCDIR=$(RTMPDIR)/src/
DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlydtl

ERL=erl
ERLC=erlc

PARSER=src/erlydtl/erlydtl_parser
APP=erlydtl.app

all: $(PARSER).erl ebin/$(APP)
	$(ERL) -make 

ebin/$(APP): src/erlydtl/$(APP)
	-mkdir -p ebin
	cp $< $@

$(PARSER).erl: $(PARSER).yrl
	$(ERLC) -o src/erlydtl src/erlydtl/erlydtl_parser.yrl
 
run:
	$(ERL) -pa ebin


test:
	$(ERL) -noshell -pa ebin \
		-s erlydtl_functional_tests run_tests \
		-s erlydtl_dateformat_tests run_tests \
		-s erlydtl_unittests run_tests \
		-s init stop
	
clean:
	rm -fv ebin/*.beam
	rm -fv ebin/$(APP)
	rm -fv erl_crash.dump $(PARSER).erl

install:
	mkdir -p $(DESTROOT)$(BEAMDIR)
	mkdir -p $(DESTROOT)$(SRCDIR)/erlydtl
	mkdir -p $(DESTROOT)$(SRCDIR)/tests
	install -c -m 644 ebin/*.beam $(DESTROOT)$(BEAMDIR)
	install -c -m 644 src/erlydtl/*.app $(DESTROOT)$(BEAMDIR)
	install -c -m 644 src/erlydtl/* $(DESTROOT)$(SRCDIR)erlydtl/
	install -c -m 644 src/tests/* $(DESTROOT)$(SRCDIR)tests/

debian:
	cp ../erlydtl_$(VERSION)*.deb $(DEBIANREPO)/binary/
	(cd $(DEBIANREPO); dpkg-scanpackages binary /dev/null | gzip -9c > binary/Packages.gz)


.PHONY: doc debian

