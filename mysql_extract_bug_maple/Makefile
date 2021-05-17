.PHONY: all clean afl random

build:
	ocamlfind opt -linkpkg -package parafuzz-lib test.ml

afl: 
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_NO_FORKSRV=1;\
	timeout 1h afl-fuzz -m none  -i ip -o op/ ./a.out @@ || true

random: 
	date ; timeout 1h ./a.out -i ; echo "" ; date

clean:
	rm -rf op/ *.cm* *.o *.out

