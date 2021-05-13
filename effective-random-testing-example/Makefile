.PHONY: all clean

all:
	ocamlfind opt -linkpkg -package parafuzz-lib test.ml

clean:
	rm -rf op/ *.cm* *.o *.out
