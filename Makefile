VERSION=`head -1 debian/changelog | sed -Ee 's/.*\(([^\)]+)\).*/\1/'`
ERLDIR=`erl -eval 'io:format("~s", [code:root_dir()])' -s init stop -noshell`/lib/http_file-$(VERSION)

DEBIANREPO=/apps/erlyvideo/debian/public
DESTROOT=$(CURDIR)/debian/erlang-http-file

all:
	erl -make

clean:
	rm -f ebin/*.beam
	
test:
	@erl -pa ebin -s http_file test -noshell -noinput -s init stop


install:
	mkdir -p $(DESTROOT)$(ERLDIR)/ebin
	install -c -m 644 ebin/*.beam ebin/*.app $(DESTROOT)$(ERLDIR)/ebin/


debian:
	dpkg-buildpackage -rfakeroot -D -i -I -S -sa
	debuild -us -uc
	cp ../erlang-http-file_$(VERSION)*.deb $(DEBIANREPO)/closed/
	rm ../erlang-http-file_$(VERSION)*
	# (cd $(DEBIANREPO)/..; ./update)

.PHONY: debian

