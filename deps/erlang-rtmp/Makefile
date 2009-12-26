all:
	erl -make
	
doc:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application   "'rtmp'" '"."' '[{def,{vsn,"1.0"}},{exclude_packages,[hmac256,rtmp_handshake,rtmp_sup,rtmp_app,rtmpt_sessions]}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

.PHONY: doc