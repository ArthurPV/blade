# to hide the commands
ifndef verbose
  SILENT = @
endif

.PHONY: all

all:
	@echo -e "\e[1;32m**\e[0m Check dependencies"
	$(SILENT)ocamlfind ocamlopt -package llvm -package ANSITerminal -package oUnit -package stdint
	@echo -e "\e[1;32m**\e[0m Build project"
	$(SILENT)dune build
	@echo -e "\e[1;32m**\e[0m Build successfuly"

test:
	@echo -e "\e[1;32m**\e[0m Run tests"
	$(SILENT)dune runtest -f

clean:
	$(SILENT)rm -rf _build
	@echo -e "\e[1;32m**\e[0m Clean project"

help:
	@echo "Lily help"
	@echo ""
	@echo "Commands:"
	@echo ""
	@echo "    all: build project"
	@echo "    test: run tests"
	@echo "    clean: clean project"
