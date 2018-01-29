./main.byte foo bar baz > tmp.txt
diff tmp.txt test1.out
./main.byte -length foo four 123456789 1 > tmp.txt
diff tmp.txt test2.out
./main.byte foo bar baz > tmp.txt
diff tmp.txt test1.out
./main.byte -length  > tmp.txt
diff tmp.txt test3.out
./main.byte > tmp.txt
diff tmp.txt test3.out
./main.byte -help > tmp.txt
diff tmp.txt test4.out
