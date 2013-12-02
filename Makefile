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
	rm -rf lib goofy sources/*.o sources/*.ppu sources/*.compiled sources/Goofy

parser_clean:
	rm -rf  sources/parser/*.o sources/parser/*.ppu sources/parser/*.compiled  sources/parser/expr.pas sources/parser/exprlex.pas

lexyacc_clean:
	rm -rf bin/ndlex bin/ndyacc

extraclean: clean lexyacc_clean
	rm -rf *.bak *~

#tests:
#	echo "sorry, can't build tests right now"
#	$(FPC) $(FPCOPTS) -otests tests.pas

run: Goofy
	./goofy -r

#runtests: clean tests
#	./tests

.PHONY: clean goofy
