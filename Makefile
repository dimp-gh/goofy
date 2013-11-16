FPC=fpc
FPCOPTS=-Mobjfpc -O1

goofy:
	$(FPC) $(FPCOPTS) -ogoofy Goofy.pas

clean:
	rm -rf lib goofy tests *.o *.ppu

extraclean: clean
	rm -rf *.bak *~

tests:
	$(FPC) $(FPCOPTS) -otests tests.pas

run: goofy
	./goofy -r
	
runtests: clean tests
	./tests

.PHONY: clean goofy
