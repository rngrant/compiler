./compiler.native test1.arith > tmp.txt~
diff tmp.txt~ test1.out
./compiler.native test2.arith > tmp.txt~
diff tmp.txt~ test2.out