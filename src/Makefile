EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLS=$(wildcard *.erl)
ERLC=erlc -o $(EBIN_DIR) -I ${INCLUDE_DIR}
BEAMS=$(ERLS:%.erl=$(EBIN_DIR)/%.beam)

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	$(ERLC) -W $<

#.yrl.erl:
#	$(ERLC)-W $<

all: compile

$(EBIN_DIR)/%.beam: %.erl
	@echo ">>" compiling: $<
	@$(ERLC) $<

generate_parser:
	@echo ">>" generating log4erl_parser.erl from log4erl_parser.yrl $<
	erlc log4erl_parser.yrl

# export LEEX_PATH env var for this to work
generate_lex:
	@echo ">>" generating log4erl_lex.erl from log4erl_lex.xrl $<
	erl -noinput -noshell -pa $$LEEX_PATH/ebin -s leex file log4erl_lex.xrl -s init stop

compile: ${BEAMS}

clean:
	rm -rf ../ebin/*.beam erl_crash.dump
