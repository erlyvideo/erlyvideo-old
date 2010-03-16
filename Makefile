all:
	erl -make
	
doc:
	erl -pa `pwd`/ebin \
	-noshell \
	-run edoc_run application   "'rtsp'" '"."' '[{def,{vsn,"1.0"}}]'

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump

clean-doc:
	rm -fv doc/*.html
	rm -fv doc/edoc-info
	rm -fv doc/*.css

.PHONY: doc