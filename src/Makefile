EBIN_DIR=../ebin
INCLUDE_DIR=../include
ERLS=$(wildcard *.erl)
ERLC=erlc -o $(EBIN_DIR) -I ${INCLUDE_DIR}
BEAMS=$(ERLS:.erl=.beam)

# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	$(ERLC) -W $<

#.yrl.erl:
#	$(ERLC)-W $<


# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character
# to the end of the line and continue on the next line
# Edit the lines below
#MODS = log4erl_sup log4erl file_appender console_appender log_manager logger_guard log4erl_utils dummy_appender log_formatter smtp_fsm.erl email_msg.erl
#MODS = $(ERLS)
BEAMS = $(ERLS:.erl=.beam)
MODS = $(BEAMS:.beam=)

all:	generate_parser generate_lex compile

%.beam: %.erl
	@echo ">>" compiling: $<
	@$(ERLC) $<

generate_parser:
	@echo ">>" generating log4erl_parser.erl from log4erl_parser.yrl $<
	erlc log4erl_parser.yrl

# export LEEX_PATH env var for this to work
generate_lex:
	@echo ">>" generating log4erl_lex.erl from log4erl_lex.xrl $<
	erl -pa $$LEEX_PATH/ebin -s leex file log4erl_lex.xrl -s init stop

compile: ${MODS:%=%.beam}

clean:
	rm -rf ../ebin/*.beam erl_crash.dump
