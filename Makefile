FPC=fpc
FPCOPTS=-Mobjfpc -O1
.PHONY = typechecker

typechecker:
	$(FPC) $(FPCOPTS) -otypechecker typechecker.pas

clean:
	rm -rf lib typechecker tests *.o *.ppu

extraclean: clean
	rm -rf *.bak *~

tests:
	$(FPC) $(FPCOPTS) -otests tests.pas

run:	typechecker
	./typechecker
	
runtests:	tests
	./tests
