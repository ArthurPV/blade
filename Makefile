builddir = build

# Objects
sources_front = $(wildcard src/front/*.ml)
header_front = $(wildcard src/front/*.mli)

# Command library

ifndef verbose
  SILENT = @
endif

.PHONY: all dune $(sources_front)

all:
	@echo -e "\e[1;32m**\e[0m Check dependencies"
	$(SILENT)ocamlfind ocamlopt -package llvm
	@echo -e "\e[1;32m**\e[0m Build project"
	$(SILENT)dune build
	@echo -e "\e[1;32m**\e[0m Build successfuly"

clean:
	$(SILENT)rm -rf _build
	@echo -e "\e[1;32m**\e[0m Clean project"
