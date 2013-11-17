TARGET    = main
TEST      = test
OPTS      = -pp camlp4o -use-ocamlfind
TAGS      = annot,debug
LIBS      =
PKGS      = ocamlgraph,oUnit,batteries
EXTENSION = byte
RUN_TEST  = ./$(TEST).$(EXTENSION)

.PHONY: all test test_bin clean

all:
	ocamlbuild $(OPTS) -tags $(TAGS) -pkgs $(PKGS) $(TARGET).$(EXTENSION)

test_bin:
	ocamlbuild $(OPTS) -tags $(TAGS) -pkgs $(PKGS) $(TEST).$(EXTENSION)

test: test_bin
	$(RUN_TEST) -k 0
	$(RUN_TEST) -k 0 -gc
	$(RUN_TEST) -k 1 -gc
	$(RUN_TEST) -k 1 # Will very likely timeout

clean:
	ocamlbuild -clean
