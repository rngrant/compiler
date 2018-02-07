./compiler.native tests/test1.arith > tmp.txt~
diff tmp.txt~ tests/test1.out
./compiler.native tests/test2.arith > tmp.txt~
diff tmp.txt~ tests/test2.out
./compiler.native tests/test3.arith > tmp.txt~
diff tmp.txt~ tests/test3.out
./compiler.native tests/test4.arith > tmp.txt~
diff tmp.txt~ tests/test4.out
./compiler.native tests/test5.arith > tmp.txt~
diff tmp.txt~ tests/test5.out
./compiler.native tests/test6.arith > tmp.txt~
diff tmp.txt~ tests/test6.out
