numTest=21

for i in `seq 1 $numTest`;
do
#    echo $i
    ./compiler.native "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".out"
    ./compiler.native -parse "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".parse.out"
done    

