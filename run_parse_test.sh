./compiler.native -parse tests/test1.arith > tmp.txt~
diff tmp.txt~ tests/test1.parse.out
./compiler.native -parse tests/test2.arith > tmp.txt~
diff tmp.txt~ tests/test2.parse.out
./compiler.native -parse tests/test3.arith > tmp.txt~
diff tmp.txt~ tests/test3.parse.out
./compiler.native -parse tests/test4.arith > tmp.txt~
diff tmp.txt~ tests/test4.parse.out
./compiler.native -parse tests/test5.arith > tmp.txt~
#diff tmp.txt~ tests/test5.parse.out
#./compiler.native -parse tests/test6.arith > tmp.txt~
#diff tmp.txt~ tests/test6.parse.out
#./compiler.native -parse tests/test7.arith > tmp.txt~
#diff tmp.txt~ tests/test7.parse.out
#./compiler.native -parse tests/test8.arith > tmp.txt~
#diff tmp.txt~ tests/test8.parse.out
#./compiler.native -parse tests/test9.arith > tmp.txt~
#diff tmp.txt~ tests/test9.parse.out
#./compiler.native -parse tests/test10.arith > tmp.txt~
#diff tmp.txt~ tests/test10.parse.out
