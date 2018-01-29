./main.native foo bar baz > tmp.txt
#echo "test 1"
diff tmp.txt test1.out
./main.native -length foo four 123456789 1 > tmp.txt
#echo "test 2"
diff tmp.txt test2.out
./main.native -length  > tmp.txt
#echo "test 3"
diff tmp.txt test3.out
./main.native > tmp.txt
diff tmp.txt test3.out
#echo "test 4"
./main.native -help > tmp.txt
diff tmp.txt test4.out
