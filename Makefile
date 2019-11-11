
build:
	@dune build @all

clean:
	@dune clean

watch:
	@dune build @all -w

test:
	@dune runtest --force --no-buffer

doc:
	@dune build @doc

install: build
	@dune build @install
	@dune install

reindent:
	@which ocp-indent || ( echo "require ocp-indent" ; exit 1 )
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 echo "reindenting: "
	@find src '(' -name '*.ml' -or -name '*.mli' ')' -type f -print0 | xargs -0 ocp-indent -i

.PHONY: all clean test tags tests
