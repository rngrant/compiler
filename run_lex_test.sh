./compiler.native -lex tests/test1.arith > tmp.txt~
diff tmp.txt~ tests/test1.lex.out
./compiler.native -lex tests/test2.arith >tmp.txt~
diff tmp.txt~ tests/test2.lex.out
./compiler.native -lex tests/test3.arith >tmp.txt~
diff tmp.txt~ tests/test3.lex.out
./compiler.native -lex tests/test4.arith >tmp.txt~
diff tmp.txt~ tests/test4.lex.out
./compiler.native -lex tests/test5.arith >tmp.txt~
diff tmp.txt~ tests/test5.lex.out
./compiler.native -lex tests/test6.arith >tmp.txt~
diff tmp.txt~ tests/test6.lex.out
./compiler.native -lex tests/test7.arith >tmp.txt~
diff tmp.txt~ tests/test7.lex.out
./compiler.native -lex tests/test8.arith >tmp.txt~
diff tmp.txt~ tests/test8.lex.out
./compiler.native -lex tests/test9.arith >tmp.txt~
diff tmp.txt~ tests/test9.lex.out
./compiler.native -lex tests/test10.arith >tmp.txt~
diff tmp.txt~ tests/test10.lex.out
