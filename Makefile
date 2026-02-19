all: build

.PHONY: build
build:
	dub build --parallel --build-mode=singleFile --compiler=ldc2 --debug debug

.PHONY: run-test
run-test:
	make -C test

.PHONY: clean
clean:
	-rm -f gidgen
	make -C test clean
