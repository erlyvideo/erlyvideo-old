all: debug

debug: clean
	erlc -d -W +debug_info amf3.erl
	mv *.beam ../ebin
	
release: 
	erlc -W amf3.erl
	mv *.beam ../ebin

docs:
	erl -run edoc file amf3.erl -run init stop -noshell	
	mv *.html ../docs

clean:
	rm -rf *.dump *.beam *.html