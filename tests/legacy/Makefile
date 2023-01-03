.PHONY: test
test:: legacy.byte

ifneq "$(shell ocamlopt -vnum 2>/dev/null)" ""
test:: legacy.native
endif

src/%:
	$(MAKE) -C src $*

legacy.byte: src/opam-file-format.cma legacy.ml
	ocamlc -o $@ -I src $^

legacy.native: src/opam-file-format.cmxa legacy.ml
	ocamlopt -o $@ -I src $^
