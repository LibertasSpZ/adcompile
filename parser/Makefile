toplevel:
	ocamllex adlexer.mll
	ocamlyacc adparser.mly
	ocamlc -c compiler.ml
	ocamlc -c ast.ml
	ocamlc -c adparser.mli
	ocamlc -c adparser.ml
	ocamlc -c adlexer.ml
	ocamlc -c main.ml
	ocamlfind ocamlmktop -o top compiler.cmo ast.cmo adparser.cmo adlexer.cmo main.cmo

exe:
	ocamllex adlexer.mll
	ocamlyacc adparser.mly
	ocamlc -c compiler.ml
	ocamlc -c ast.ml
	ocamlc -c adparser.mli
	ocamlc -c adparser.ml
	ocamlc -c adlexer.ml
	ocamlc -c main.ml
	ocamlfind ocamlc -o adcompile compiler.cmo ast.cmo adparser.cmo adlexer.cmo main.cmo


clean:
	rm *.cmo *.cmi *.mli top
