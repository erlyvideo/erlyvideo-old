SRC_ERLS = `find src -name '*.erl'`
TEST_ERLS = `find test -name '*.erl'`
APPS = `find src -name '*.app'`
COPY_APPS = for APP in $(APPS); do cp $$APP ebin; done

all: 
	@ $(COPY_APPS)
	@ erl -make

debug: 
	@ make tmp
	@ cd tmp; erlc +debug_info *.erl; cp amf0.beam amf3.beam ../ebin
	
tests: 
	@ make clean debug
	@ cd tmp; erl -noshell -eval "eunit:test([amf0,amf3],[verbose])" -s init stop
	
cover:
	@ make clean debug	
	@ cd tmp; erl -noshell -eval "test_coverage:analyze([amf0, amf3])" -s init stop
	
dialyzer:
	@ make clean debug	
	@ cd tmp; dialyzer --build_plt -r "."
	
clean:
	@ rm -rf tmp ebin/* 

tmp:
	@ mkdir tmp;
	@ cp $(SRC_ERLS) tmp/
	@ cp $(TEST_ERLS) tmp/