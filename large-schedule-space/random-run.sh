#/bin/bash
for i in `seq 10 10 100`
do

    for j in {1..5}
    do
        echo $i $j
        date; timeout --foreground 1h ./$i.out -i || true; echo ""; date
        echo "----------------------"
        echo ""
        #make random 2>&1  >> random-run.txt
    done
done

