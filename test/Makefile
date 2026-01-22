.PHONY: gidgen-test
gidgen-test: gidgen-test/gidgen-test

gidgen-test/gidgen-test: gidgen-test/libgidgen-test.so.0 gidgen-test/gidgen-test.d packages/README.md
	cd gidgen-test && dub build --compiler=ldc2 --debug debug --build=unittest

TEST_CFLAGS = $(shell pkg-config --cflags glib-2.0)
TEST_LDFLAGS = $(shell pkg-config --libs glib-2.0)

gidgen-test/libgidgen-test.so.0: gidgen-test/gidgen-test.c
	gcc -shared -fPIC -o $@ $(TEST_CFLAGS) $< $(TEST_LDFLAGS)

gidgen-test/gidgen-test.c gidgen-test/gidgen-test.d gir/GidgenTest-1.0.gir: generate-test-files
	@:

generate-test-files: gidgen-test-gen/gidgen-test-gen
	gidgen-test-gen/gidgen-test-gen

packages/README.md: gir/GidgenTest-1.0.gir
	dub run gidgen -- --defs defs --gir-path gir --pkg-path . --subpkg-path packages --log-level=error

gidgen-test-gen/gidgen-test-gen: gidgen-test-gen/app.d
	cd gidgen-test-gen && dub build --compiler=ldc2 --debug debug

.PHONY: clean
clean:
	-rm -f gidgen-test/gidgen-test.* gidgen-test/libgidgen-test.so gir/GidgenTest-1.0.gir gidgen-test-gin/gidgen-test-gen gidgen-test/gidgen-test
	-rm -rf packages
