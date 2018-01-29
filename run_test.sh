./main.native foo bar baz > tmp.txt
diff tmp.txt test1.out
./main.native -length foo four 123456789 1 > tmp.txt
diff tmp.txt test2.out
./main.native foo bar baz > tmp.txt
diff tmp.txt test1.out
./main.native -length  > tmp.txt
diff tmp.txt test3.out
./main.native > tmp.txt
diff tmp.txt test3.out
./main.native -help > tmp.txt
diff tmp.txt test4.out
