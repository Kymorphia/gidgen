all: build

.PHONY: build
build:
	dub build --parallel --build-mode=singleFile --compiler=ldc2 --debug debug

run-test: gidgen-test
	LD_LIBRARY_PATH=test/gidgen-test test/gidgen-test/gidgen-test

.PHONY: gidgen-test
gidgen-test:
	make -C test

.PHONY: clean
clean:
	-rm -f gidgen
	make -C test clean
