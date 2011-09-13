EBIN_DIR=ebin
EXBIN_DIR=exbin
TEST_DIR=test
INCLUDE_DIR=include

TEST_SOURCE_DIR=$(TEST_DIR)/erlang
TEST_EBIN_DIR=$(TEST_DIR)/ebin

ERLC=erlc -I $(INCLUDE_DIR) -W0
ERL=erl -I $(INCLUDE_DIR) -noshell -pa $(EBIN_DIR)

.PHONY: test exbin test_erlang test_elixir clean clean_exbin

# This is the default task
compile:
	@ ./rebar compile
	@ make exbin

# install:
# We will need to do this one at some point

exbin: lib/*.ex lib/*/*.ex
	@ echo Compiling Elixir source ...
	@ mkdir -p $(EXBIN_DIR)
	@ touch $(EXBIN_DIR)
	$(ERL) -s elixir_compiler core -s erlang halt
	@ echo

test_erlang: compile
	@ echo Running Erlang tests ...
	@ mkdir -p $(TEST_EBIN_DIR)
	@ # Compile test files
	@ $(ERLC) -o $(TEST_EBIN_DIR) $(TEST_SOURCE_DIR)/*.erl
	@ # Look and execute each file
	time $(ERL) $(TEST_EBIN_DIR) -pa exbin -s test_helper test -s erlang halt
	@ echo

test_elixir: compile
	@ echo Running Elixir tests ...
	time bin/exunit test/elixir/*_test.exs test/elixir/*/*_test.exs
	@ echo

test: test_erlang test_elixir

clean: clean_exbin
	rm -f src/elixir_lexer.erl
	rm -f src/elixir_parser.erl
	rm -f src/eex_lexer.erl
	rm -rf $(EBIN_DIR)/*.beam
	rm -rf $(TEST_EBIN_DIR)/*.beam
	@ echo

clean_exbin:
	rm -rf $(EXBIN_DIR)