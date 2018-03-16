numTest=36

for i in `seq 1 $numTest`;
do
#   echo $i
#   echo "out"
    ./compiler.native "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".out"
#    echo "parse"    
    ./compiler.native -parse "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".parse.out"
#    echo "type"    
    ./compiler.native -typecheck "tests/test"$i".arith" > tmp.txt~
    diff tmp.txt~ "tests/test"$i".typ"
done    

