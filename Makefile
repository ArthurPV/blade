all:
	opam install color llvm
	dune build

clean:
	rm -rf _build
