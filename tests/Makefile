all: debug

debug: 
	erlc -d -W +debug_info amf3_tests.erl
	mv *.beam ../ebin
	
release: 
	erlc -W amf3_tests.erl
	mv *.beam ../ebin

test: 
	cd ../ebin; erl -noshell -eval "eunit:test(amf3, [verbose])" -s init stop

clean:
	rm -rf *.dump *.beam	