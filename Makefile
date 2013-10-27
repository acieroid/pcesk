TARGET    = main
TEST      = test
OPTS      = -pp camlp4o -use-ocamlfind
TAGS      = annot,debug
LIBS      = str
PKGS      = ocamlgraph,oUnit
EXTENSION = byte

.PHONY: all test test_bin clean

all:
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) -pkgs $(PKGS) $(TARGET).$(EXTENSION)

test_bin:
	ocamlbuild $(OPTS) -tags $(TAGS) -libs $(LIBS) -pkgs $(PKGS) $(TEST).$(EXTENSION)

test: test_bin
	./$(TEST).$(EXTENSION)

clean:
	ocamlbuild -clean
