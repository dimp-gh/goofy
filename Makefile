FPC=fpc
FPCOPTS=-Mobjfpc -O1
.PHONY = typechecker

typechecker:
	$(FPC) $(FPCOPTS) -otypechecker typechecker.pas

clean:
	rm -rf lib typechecker

extraclean: clean
	rm -rf *.bak *~
