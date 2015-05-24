
EXEC=camlart
OCAMLC=ocamlc

all:
	$(OCAMLC) Expr.mli Expr.ml
	$(OCAMLC) graphicsPs.ml
	$(OCAMLC) graphics.cma graphicsPs.cmo Expr.cmo Draw.mli Draw.ml
	$(OCAMLC) graphics.cma graphicsPs.cmo Expr.cmo Draw.cmo camlart.mli camlart.ml -o $(EXEC)
	rm -rf *.cmi *.cmo *.out
