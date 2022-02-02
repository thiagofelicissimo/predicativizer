all:
	ocamlc -o program str.cma unif.ml

clean:
	rm program
