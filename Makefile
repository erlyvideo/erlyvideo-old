all: debug
debug: compile-debug test analyze
release: compile-release documentation test

analyze: compile-debug
	@echo "\n\n======================================================="
	@echo "  Analyzing with Dialyzer ..."
	@echo "=======================================================\n"	
	cd ebin; dialyzer --build_plt -r "."
	@echo "\n"

documentation:
	@echo "\n\n======================================================="
	@echo "  Creating Documentation ..."
	@echo "=======================================================\n"	
	mkdir docs
	cd src; make docs	
	@echo "\n"

test:
	@echo "\n\n======================================================="
	@echo "  Running Tests ..."
	@echo "=======================================================\n"	
	cd tests; make test
	@echo "\n"

compile-debug: clean
	@echo "\n\n======================================================="
	@echo "  Compiling in debug mode ..."
	@echo "=======================================================\n"	
	mkdir ebin;
	cd src; make debug	
	cd tests; make debug
	@echo "\n"

compile-release: clean  
	@echo "\n\n======================================================="
	@echo "  Compiling ..."
	@echo "=======================================================\n"	
	mkdir ebin;
	cd src; make release	
	cd tests; make release
	@echo "\n"
	
clean:
	@echo "\n\n======================================================="
	@echo "  Cleaning ..."
	@echo "=======================================================\n"	
	rm -rf ebin
	rm -rf docs
	rm -rf *.dump *.beam
	cd src; make clean
	cd tests; make clean
	@echo "\n"