INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

TARGETS_LIB = choice.cmxa choice.cma choice.a choice.cmi choice.cmxs
TARGETS_DOC = choice.docdir/index.html
INSTALL = $(addprefix _build/, $(TARGETS_LIB)) choice.mli

OPTIONS = -use-ocamlfind
	
all: lib

lib:
	ocamlbuild $(OPTIONS) $(TARGETS_LIB) $(TARGETS_DOC)

doc:
	ocamlbuild $(OPTIONS) $(TARGETS_DOC)

clean:
	ocamlbuild -clean

tests:
	ocamlbuild -package oUnit $(OPTIONS) tests/run_tests.native

install:
	ocamlfind install choice META $(INSTALL)

push_doc: all
	scp -r choice.docdir/* cedeela.fr:~/simon/root/software/choice/

uninstall:
	ocamlfind remove choice

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags push_doc tests
