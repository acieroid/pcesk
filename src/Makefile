TARGET    = main
OPTS      = -pp camlp4o
TAGS      = annot,debug
LIBS      = str
EXTENSION = byte

all:
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) $(TARGET).$(EXTENSION)

clean:
	ocamlbuild -clean
