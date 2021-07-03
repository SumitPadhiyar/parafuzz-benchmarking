#/bin/bash
for i in {1..2}
do
    make stress >> stress-run.txt
done
