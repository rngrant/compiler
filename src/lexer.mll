(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/lexer.mll *)

rule token = parse
  | [' ' '\t' '\n']         { token lexbuf }     (* skip blanks *)
  | ['0'-'9']+ as lxm       { Parser.INT (int_of_string lxm) }
  | '+'                     { Parser.PLUS }
  | '-'                     { Parser.MINUS }
  | '*'                     { Parser.TIMES }
  | '/'                     { Parser.DIV }
  | '('                     { Parser.LPAREN }
  | ')'                     { Parser.RPAREN }
  | eof                     { Parser.EOF }
  | _  { failwith "don't know how to handle '%s'" (Lexing.lexeme lexbuf) }
