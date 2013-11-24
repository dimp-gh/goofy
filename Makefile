FPC=fpc
FPCOPTS=-Mobjfpc -O1

Goofy:
	lazbuild Goofy.lpr

clean:
	rm -rf lib Goofy tests *.o *.ppu *.compiled

extraclean: clean
	rm -rf *.bak *~

tests:
	echo "sorry, can't build tests right now"
#	$(FPC) $(FPCOPTS) -otests tests.pas

run: Goofy
	./Goofy -r
	
runtests: clean tests
	./tests

.PHONY: clean goofy
