.PHONY: all clean

all:
	ocamlfind opt -thread -afl-instrument -linkpkg -package parafuzz-lib *.ml 

clean:
	rm -rf op/ *.cm* *.o *.out
