all: build

build:
	dune build
	cp -f _build/default/bin/main.exe ./Fhree

install: build 
	mkdir -p _install
	cp -f ./LICENSE _install/LICENSE
	cp -f ./README.md _install/README.md
	cp -f _build/default/bin/main.exe _install/main.exe

clear: clean

clean:
	rm -fr _build
	rm -fr _install
	rm -f ./Fhree

.PHONY: all build install clear clean