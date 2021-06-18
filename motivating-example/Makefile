.PHONY: build clean afl random afli stress_build stress

stress: 
	timeout --foreground 1h ./stress_test.out -i || true

build:
	ocamlfind opt -linkpkg -package parafuzz-lib test.ml -o test.out

stress_build:
	ocamlfind opt -linkpkg -package crowbar stress_test.ml -o stress_test.out

afl: 
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_NO_FORKSRV=1;\
	timeout --foreground 1h  afl-fuzz -m none -d -i ip -o op/ ./test.out @@ || true

afli: 
	export AFL_BENCH_UNTIL_CRASH=1; \
	export AFL_SKIP_CPUFREQ=1; \
	export AFL_NO_FORKSRV=1;\
	timeout --foreground 1h  afl-fuzz -m none -i ip -o op/ ./test.out @@ || true

random: 
	date; timeout --foreground 1h ./test.out -i; echo ""; date

clean:
	rm -rf op/ *.cm* *.o *.out

