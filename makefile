TARGET=src/main

#ANALYZER=src/analyzer

default: $(TARGET).native

$TARGET: default

native: $(TARGET).native

%.native:
	ocamlbuild -use-menhir -menhir "menhir --explain" -use-ocamlfind -package ppx_deriving.std $@
	mv main.native ./Fhree

#analyzer: src/analyzer.native

#%.native:
#	ocamlbuild -use-ocamlfind -package ppx_deriving.std -use-menhir $@

clean:
	ocamlbuild -clean

.PHONY: clean default
