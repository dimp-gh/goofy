SHELL := /bin/bash

Goofy: lexer parser
	lazbuild sources/Goofy.lpr
	cp sources/Goofy ./goofy

parser:
	pushd sources/parser && \
	../../bin/ndyacc expr.y && \
	popd

lexer:
	pushd sources/parser && \
	../../bin/ndlex exprlex.l && \
	popd

clean: parser_clean
	rm -rf lib goofy sources/*.o sources/*.ppu sources/*.compiled sources/Goofy lang/*.actual

parser_clean:
	rm -rf  sources/parser/*.o sources/parser/*.ppu sources/parser/*.compiled  sources/parser/expr.pas sources/parser/exprlex.pas

lexyacc_clean:
	rm -rf bin/ndlex bin/ndyacc

extraclean: clean lexyacc_clean
	rm -rf *.bak *~

cleantests:
	rm -rf  tests/*.o tests/*.ppu tests/*.compiled tests/Tests

buildtests:
	lazbuild tests/Tests.lpr

run: Goofy
	./goofy -r

runtests: cleantests buildtests
	./tests/Tests

.PHONY: clean goofy
