all: debug

debug: 
	erlc -d -W +debug_info amf3_tests.erl test_coverage.erl
	mv *.beam ../build
	
release: 
	erlc -W amf3_tests.erl
	mv *.beam ../build

test: 
	cd ../build; erl -noshell -eval "eunit:test(amf3, [verbose])" -s init stop

coverage: 
	cp ../src/amf3.erl ../build
	cd ../build; erl -noshell -eval "test_coverage:analyze(amf3)" -s init stop

clean:
	rm -rf *.dump *.beam	