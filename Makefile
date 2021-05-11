ifndef verbose
  SILENT = @
endif

.PHONY: all

all:
	@echo -e "\e[1;32m**\e[0m Check dependencies"
	$(SILENT)ocamlfind ocamlopt -package llvm -package containers -package ANSITerminal -package oUnit
	@echo -e "\e[1;32m**\e[0m Build project"
	$(SILENT)dune build
	@echo -e "\e[1;32m**\e[0m Build successfuly"

test:
	@echo -e "\e[1;32m**\e[0m Run tests"
	$(SILENT)dune runtest

clean:
	$(SILENT)rm -rf _build
	@echo -e "\e[1;32m**\e[0m Clean project"
