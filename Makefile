all:
	erl -make

clean:
	rm -f ebin/*.beam
	
test:
	@erl -pa ebin -s http_file test -noshell -noinput -s init stop
	