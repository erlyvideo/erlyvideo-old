ERL=erl
APP_NAME=ems
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)@`hostname`
VSN=0.1

include support/include.mk

all: $(EBIN_FILES)
	echo $(EBIN_FILES)

doc:	
	$(ERL) -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application  "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

run:
	[ -f ebin/erlmedia.app ] || cp src/erlmedia.app ebin/erlmedia.app
	$(ERL) -pa `pwd`/ebin -pa `pwd`/deps/*/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME)
	
start:
	[ -f `pwd`/ebin/erlmedia.app ] || cp src/erlmedia.app ebin/erlmedia.app
	$(ERL) -pa `pwd`/ebin -pa `pwd`/deps/*/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME) \
	-detached
