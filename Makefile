.PHONY = all

all:
	lazbuild typechecker.lpr

clean:
	rm -r lib typechecker
