all:
	opam install ANSITerminal llvm
	dune build

clean:
	rm -rf _build
