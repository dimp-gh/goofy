.PHONY = clean all

all:
	lazbuild typechecker.lpr

clean:
	rm -rf lib typechecker

extraclean: clean
	rm -rf *.bak *~
