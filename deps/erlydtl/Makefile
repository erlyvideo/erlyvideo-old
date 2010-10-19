ERLDIR=$(ERLANG_ROOT)/lib/erlydtl-$(VERSION)

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
	rm -fv erl_crash.dump $(PARSER).erl

install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	mkdir -p $(DESTROOT)$(ERLDIR)/src/erlydtl
	mkdir -p $(DESTROOT)$(ERLDIR)/src/tests
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 src/erlydtl/* $(DESTROOT)$(ERLDIR)/src/erlydtl/
	install -c -m 644 src/tests/* $(DESTROOT)$(ERLDIR)/src/tests/


.PHONY: doc

