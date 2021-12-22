.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

trouble:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f trouble.zip
	zip -r trouble.zip . -x@exclude.lst

clean:
	dune clean

docs: 
	dune build @doc 