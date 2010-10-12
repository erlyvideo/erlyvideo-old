ERLDIR=$(ERLANG_ROOT)/lib/erlmedia-$(VERSION)
DESTROOT=$(CURDIR)/debian/amf

all: compile

compile:
	@ ./rebar compile
	
clean:
	@ ./rebar clean

test:
	@ ./rebar eunit
	
install:
  mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.beam $(DESTROOT)$(ERLDIR)/ebin/
	