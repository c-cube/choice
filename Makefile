INTERFACE_FILES = $(shell find src -name '*.mli')
IMPLEMENTATION_FILES = $(shell find src -name '*.ml')

CMT_FILES = $(wildcard _build/src/*.cmt) $(wildcard _build/src/*.cmti)
TARGETS_LIB = $(addprefix src/, choice.cmxa choice.cma choice.a choice.cmi choice.cmxs)
TARGETS_DOC = choice.docdir/index.html
INSTALL = $(addprefix _build/, $(TARGETS_LIB)) $(CMT_FILES) $(IMPLEMENTATION_FILES) $(INTERFACE_FILES)

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

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: all clean tests tags push_doc tests
