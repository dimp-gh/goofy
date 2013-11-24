FPC=fpc
FPCOPTS=-Mobjfpc -O1

Goofy:
	lazbuild sources/Goofy.lpr
	cp sources/Goofy ./goofy 

clean:
	rm -rf lib goofy tests sources/*.o sources/*.ppu sources/*.compiled sources/Goofy

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
