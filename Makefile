all:
	ocamlc -o program str.cma unif.ml

translate:
	bash translate.sh

clean:
	rm program

