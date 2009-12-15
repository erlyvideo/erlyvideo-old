all: debug
debug: clean compile-debug test
release: clean compile-release documentation test

analyze:
	@echo "\n\n======================================================="
	@echo "  Analyzing with Dialyzer ..."
	@echo "=======================================================\n"	
	cd ebin; dialyzer --build_plt -r "."
	@echo "\n"

documentation:
	@echo "\n\n======================================================="
	@echo "  Creating Documentation ..."
	@echo "=======================================================\n"	
	cd src; make docs	
	@echo "\n"

test:
	@echo "\n\n======================================================="
	@echo "  Running Tests ..."
	@echo "=======================================================\n"	
	cd tests; make test
	@echo "\n"

compile-debug:
	@echo "\n\n======================================================="
	@echo "  Compiling in debug mode ..."
	@echo "=======================================================\n"	
	cd src; make debug	
	cd tests; make debug
	@echo "\n"

compile-release:   
	@echo "\n\n======================================================="
	@echo "  Compiling ..."
	@echo "=======================================================\n"	
	cd src; make release	
	cd tests; make release
	@echo "\n"
	
clean:
	@echo "\n\n======================================================="
	@echo "  Cleaning ..."
	@echo "=======================================================\n"	
	rm -rf ebin/*
	rm -rf docs/*.html
	rm -rf *.dump *.beam
	cd src; make clean
	cd tests; make clean
	@echo "\n"