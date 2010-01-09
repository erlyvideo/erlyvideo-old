SRC_ERLS = `find src -name '*.erl'`
TEST_ERLS = `find test -name '*.erl'`
LINE = "\\n=================================================================\\n"
MSG = "\n$(LINE) $1 $(LINE)"

all: 
	make compile-release 
	make docs 
	make tests
	rm -rf tmp
	@echo "\n"
	
debug: 
	make compile-debug
	make tests-verbose 
	make analyze
	@echo "\n"
	
compile-debug: clean
	@echo $(call MSG,"Compiling in debug mode ...")
	make compile-setup
	cd tmp; erlc -d -W +debug_info *.erl; cp amf0.beam amf3.beam ../ebin

compile-release: clean	
	@echo $(call MSG,"Compiling ...")
	make compile-setup
	cd tmp; erlc -W *.erl; cp amf0.beam amf3.beam ../ebin

analyze: compile-debug
	make analyze-coverage
	make analyze-dialyzer

analyze-coverage:
	@echo $(call MSG,"Analyzing Code Coverage ...")
	cd tmp; erl -noshell -eval "test_coverage:analyze([amf0, amf3])" -s init stop

analyze-dialyzer:
	@echo $(call MSG,"Analyzing with Dialyzer ...")
	cd tmp; dialyzer --build_plt -r "."
	rm -rf tmp/*.erl
	
docs:
	@echo $(call MSG,"Creating Documentation ...")
	mkdir docs
	cd tmp; erl -run edoc file amf3.erl -run init stop -noshell 
	mv tmp/*.html docs
	
tests:
	@echo $(call MSG,"Running Tests ...")
	cd tmp; erl -noshell -eval "eunit:test([amf0,amf3])" -s init stop

tests-verbose:
	@echo $(call MSG,"Running Tests ...")
	cd tmp; erl -noshell -eval "eunit:test(amf3,[verbose])" -s init stop
	
clean:
	@echo $(call MSG,"Cleaning ...")
	rm -rf ebin tmp docs
	@echo "\n"
	
compile-setup:
	mkdir ebin tmp;
	cp $(SRC_ERLS) tmp/
	cp $(TEST_ERLS) tmp/	