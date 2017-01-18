INTERFACE_FILES = $(shell find -name '*.mli')
IMPLEMENTATION_FILES = $(shell find -name '*.ml')

CMT_FILES = $(wildcard _build/*.cmt) $(wildcard _build/*.cmti)
TARGETS_LIB = choice.cmxa choice.cma choice.a choice.cmi choice.cmxs
TARGETS_DOC = choice.docdir/index.html
INSTALL = $(addprefix _build/, $(TARGETS_LIB)) $(CMT_FILES) choice.mli

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

bench:
	ocamlbuild -I src $(OPTIONS) -package containers tests/bench.native

install:
	ocamlfind install choice META $(INSTALL)

push_doc: all
	scp -r choice.docdir/* cedeela.fr:~/simon/root/software/choice/

uninstall:
	ocamlfind remove choice

tags:
	otags *.ml *.mli

.PHONY: all clean tests tags push_doc tests
