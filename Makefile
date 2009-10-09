ERL=erl +bin_opt_info +debug
APP_NAME=ems
MNESIA_DATA=mnesia-data
NODE_NAME=$(APP_NAME)@`hostname`
VSN=0.1

include support/include.mk

all: ebin/erlmedia.app
	erl -make


ebin/erlmedia.app:
	cp src/erlmedia.app ebin/erlmedia.app

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

run: ebin/erlmedia.app
	$(ERL) \
	+A 4 +K true \
	-pa `pwd`/ebin -pa `pwd`/deps/*/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME)
	
start: ebin/erlmedia.app
	$(ERL) -pa `pwd`/ebin -pa `pwd`/deps/*/ebin \
	-boot start_sasl \
	-s $(APP_NAME) \
	-mnesia dir "\"${MNESIA_DATA}\"" \
	-name $(NODE_NAME) \
	-detached
