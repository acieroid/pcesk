TARGET     = main
TEST       = test
OPTS       = -pp camlp4o -use-ocamlfind
TAGS       = annot,debug
LIBS       =
PKGS       = ocamlgraph,oUnit,batteries
EXTENSION  = byte
RUN_TEST   = ./$(TEST).$(EXTENSION)
DOCDIR     = pcesk.docdir
CFLAGS     = -w A -w -4 -w -27 -short-paths
OCAMLBUILD = ocamlbuild $(OPTS) -tags $(TAGS) -pkgs $(PKGS) -cflags "$(CFLAGS)"

.PHONY: all test test_bin clean

all:
	$(OCAMLBUILD) $(TARGET).$(EXTENSION)

test_bin:
	$(OCAMLBUILD) $(TEST).$(EXTENSION)

test: test_bin
	$(RUN_TEST) -k 0 -gc
	$(RUN_TEST) -k 0
	$(RUN_TEST) -k 1 -gc
	$(RUN_TEST) -k 1 # Will very likely timeout

doc:
	$(OCAMLBUILD) $(DOCDIR)/index.html

clean:
	$(OCAMLBUILD) -clean
