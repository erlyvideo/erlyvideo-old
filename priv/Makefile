GCC=gcc
LDFLAGS=-shared -L/usr/local/lib -bundle -flat_namespace -undefined suppress -fPIC
SRCS=child_control.c child_control.h
OUTPUT=child_control.so
CFLAGS=-o ../ebin/${OUTPUT} -I/usr/local/lib/erlang/usr/include/ -arch x86_64

all: ../ebin/${OUTPUT}


../ebin/${OUTPUT}: child_control.c child_control.h
	${GCC} child_control.c ${LDFLAGS} ${CFLAGS}

clean:
	rm -rf ${OUTPUT}
