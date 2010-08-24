include debian/version.mk
ERLANG_ROOT := $(shell erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell)
PACKAGE_DIR = $(ERLANG_ROOT)/lib/erlydtl-$(VERSION)
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
	mkdir -p $(DESTROOT)$(PACKAGE_DIR)/ebin
	mkdir -p $(DESTROOT)$(PACKAGE_DIR)/src/erlydtl
	mkdir -p $(DESTROOT)$(PACKAGE_DIR)/src/tests
	install -c -m 644 ebin/*.beam $(DESTROOT)$(PACKAGE_DIR)/ebin
	install -c -m 644 src/erlydtl/*.app $(DESTROOT)$(PACKAGE_DIR)/ebin
	install -c -m 644 src/erlydtl/* $(DESTROOT)$(PACKAGE_DIR)/src/erlydtl/
	install -c -m 644 src/tests/* $(DESTROOT)$(PACKAGE_DIR)/src/tests/


.PHONY: doc

