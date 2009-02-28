# leave these lines alone
.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -o $(EBIN_DIR) -W $<

.yrl.erl:
	erlc -o $(EBIN_DIR) -W $<

SOURCE_DIR=src
EBIN_DIR=../ebin
INCLUDE_DIR=include
#SRC=$(wildcard $(SOURCE_DIR)/*)
ERL = erl -o $(EBIN_DIR) -I ${INCLUDE_DIR}
SRC = src

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character
# to the end of the line and continue on the next line
# Edit the lines below
MODS = log4erl_sup log4erl file_appender console_appender log_manager logger_guard log4erl_utils dummy_appender log_formatter

all:	compile

#compile: ${MODS:%=%.beam} #application
#compile: ${SRC}/${MODS:%=%.beam}
compile: ${MODS:%=%.beam}

## special compilation requirements are added here
#.beam: 
#	${ERL} -W0 *.erl

application: compile
#	${ERL} -pa ebin -o ${EBIN_DIR} -s application start Arg1 Arg2

run:
	${ERL} -pa ebin -s test test
# the subdirs target compiles any code in
# sub-directories
#subdirs:
#	cd dir1; make

# remove all the code
clean:
	rm -rf ../ebin/*.beam erl_crash.dump
#	cd dir1; make clean
