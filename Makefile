TARGET    = main
OPTS      = -pp camlp4o -use-ocamlfind
TAGS      = annot,debug
LIBS      = str
PKGS      = ocamlgraph
EXTENSION = byte

all:
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) -pkgs $(PKGS) $(TARGET).$(EXTENSION)

clean:
	ocamlbuild -clean
