.PHONY: all clean

all:
	ocamlfind opt -afl-instrument -linkpkg -package parafuzz-lib flightTicket.ml flight.ml flightsTable.ml airline.ml customer.ml seller.ml test.ml

clean:
	rm -rf op/ *.cm* *.o *.out
