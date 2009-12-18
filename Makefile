all: debug
debug: compile-debug test analyze
release: compile-release documentation test

compile-debug: clean
	@echo "\n\n======================================================="
	@echo "	 Compiling in debug mode ..."
	@echo "=======================================================\n" 
	mkdir ebin;
	mkdir build;
	cd src; make debug	
	cd tests; make debug
	@echo "\n"

compile-release: clean	
	@echo "\n\n======================================================="
	@echo "	 Compiling ..."
	@echo "=======================================================\n" 
	mkdir ebin;
	mkdir build;
	cd src; make release	
	cd tests; make release
	@echo "\n"

analyze: compile-debug
	@echo "\n\n======================================================="
	@echo "	 Analyzing Code Coverage ..."
	@echo "=======================================================\n"
	cd tests; make coverage	 
	@echo "\n\n======================================================="
	@echo "	 Analyzing with Dialyzer ..."
	@echo "=======================================================\n" 
	cd build; dialyzer --build_plt -r "."
	@echo "\n"

documentation:
	@echo "\n\n======================================================="
	@echo "	 Creating Documentation ..."
	@echo "=======================================================\n" 
	mkdir docs
	cd src; make docs 
	@echo "\n"

test:
	@echo "\n\n======================================================="
	@echo "	 Running Tests ..."
	@echo "=======================================================\n" 
	cd tests; make test
	@echo "\n"
	
clean:
	@echo "\n\n======================================================="
	@echo "	 Cleaning ..."
	@echo "=======================================================\n" 
	rm -rf ebin
	rm -rf build
	rm -rf docs
	rm -rf *.dump *.beam
	cd src; make clean
	cd tests; make clean
	@echo "\n"