for i in `seq 1 $1`;
do
    ./compiler.native "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".out"
done    

