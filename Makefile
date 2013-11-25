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

clean:
	rm -rf lib goofy tests sources/{parser/,}*.o sources/{parser/,}*.ppu sources/{parser/,}*.compiled sources/Goofy sources/parser/expr.pas sources/parser/exprlex.pas

extraclean: clean
	rm -rf *.bak *~

tests:
	echo "sorry, can't build tests right now"
#	$(FPC) $(FPCOPTS) -otests tests.pas

run: Goofy
	./goofy -r

runtests: clean tests
	./tests

.PHONY: clean goofy
