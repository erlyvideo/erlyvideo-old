ERL_ROOT=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`

all: compile

compile: ebin/mpeg2_crc32.so ebin/mpegts_reader.so
	erl -make
	
ebin/mpeg2_crc32.so: src/mpeg2_crc32.c
	gcc  -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall -o $@ $< -I $(ERL_ROOT)/usr/include/ || touch $@

ebin/mpegts_reader.so: src/mpegts_reader.c
	gcc  -O3 -fPIC -bundle -flat_namespace -undefined suppress -fno-common -Wall -o $@ $< -I $(ERL_ROOT)/usr/include/ || touch $@


clean:
	rm -fv ebin/*.beam ebin/*.so
	rm -f erl_crash.dump
	