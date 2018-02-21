(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/lexer.mli *)

val token: Lexing.lexbuf -> Parser.token
