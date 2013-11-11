TARGET    = main
TEST      = test
OPTS      = -pp camlp4o -use-ocamlfind
TAGS      = annot,debug
LIBS      =
PKGS      = ocamlgraph,oUnit,batteries
EXTENSION = byte

.PHONY: all test test_bin clean

all:
	ocamlbuild $(OPTS) -tags $(TAGS) -pkgs $(PKGS) $(TARGET).$(EXTENSION)

test_bin:
	ocamlbuild $(OPTS) -tags $(TAGS) -pkgs $(PKGS) $(TEST).$(EXTENSION)

test: test_bin
	./$(TEST).$(EXTENSION) -k 0
	./$(TEST).$(EXTENSION) -k 1

clean:
	ocamlbuild -clean
