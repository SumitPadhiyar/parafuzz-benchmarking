#/bin/bash
for i in {1..5}
do
    make random 2>&1  >> random-run.txt
done
