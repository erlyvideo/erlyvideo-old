SRC_ERLS = `find src -name '*.erl'`
TEST_ERLS = `find test -name '*.erl'`
LINE = "\\n=================================================================\\n"
MSG = "\n$(LINE) $1 $(LINE)"

all: 
	make compile-release 
	make docs 
	make tests
	rm -rf tmp
	
debug: 
	make compile-debug
	make tests-verbose 
	make analyze
	
compile-debug: clean
	@echo "Compiling in debug mode ..."
	make compile-setup
	cd tmp; erlc -d -W +debug_info *.erl; cp amf0.beam amf3.beam ../ebin

compile-release: clean	
	@echo "Compiling ..."
	make compile-setup
	cd tmp; erlc -W *.erl; cp amf0.beam amf3.beam ../ebin

analyze: compile-debug
	make analyze-coverage
	make analyze-dialyzer

analyze-coverage:
	@echo "Analyzing Code Coverage ..."
	cd tmp; erl -noshell -eval "test_coverage:analyze([amf0, amf3])" -s init stop

analyze-dialyzer:
	@echo "Analyzing with Dialyzer ..."
	cd tmp; dialyzer --build_plt -r "."
	
docs:
	@echo "Creating Documentation ..."
	mkdir docs
	cd tmp; erl -run edoc file amf3.erl -run init stop -noshell 
	mv tmp/*.html docs
	
tests:
	@echo "Running Tests ..."
	cd tmp; erl -noshell -eval "eunit:test([amf0,amf3])" -s init stop

tests-verbose:
	@echo "Running Tests ..."
	cd tmp; erl -noshell -eval "eunit:test(amf3,[verbose])" -s init stop
	
clean:
	@echo "Cleaning ..."
	rm -rf ebin tmp docs
	
compile-setup:
	mkdir ebin tmp;
	cp $(SRC_ERLS) tmp/
	cp $(TEST_ERLS) tmp/	