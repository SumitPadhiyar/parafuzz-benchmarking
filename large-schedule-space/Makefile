.PHONY: all clean afl random afli

build:
	ocamlfind opt -linkpkg -package parafuzz-lib test.ml

afl: 
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_NO_FORKSRV=1;\
	timeout --foreground 1h  afl-fuzz -m none -d -i ip -o op/ ./a.out @@ || true

afli: 
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_NO_FORKSRV=1;\
	timeout --foreground 1h  afl-fuzz -m none -i ip -o op/ ./a.out @@ || true

random: 
	date; timeout --foreground 1h ./a.out -i; echo ""; date

clean:
	rm -rf op/ *.cm* *.o *.out

