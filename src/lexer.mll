(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/lexer.mll 
 * And
 * https://github.com/psosera/csc312-example-compiler/...
 * blob/parser-gen/ocaml/src/lexer.mll
*)
{
open Lexing
open Parser

exception Lexer_error of string

let symbols : (string * Parser.token) list =
  [ ("(", LPAREN)
  ; (")", RPAREN)
  ; ("+", PLUS)
  ; ("-", MINUS)
  ; ("*", TIMES)
  ; ("/", DIV)
  ]

let create_symbol lexbuf =
  let str = lexeme lexbuf in
  List.assoc str symbols

let create_int lexbuf = lexeme lexbuf |> int_of_string
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let symbol_reg = '(' | ')' | '+' | '-' | '*' | '/' | '<' | '='

rule token = parse
  | whitespace+ | newline+  { token lexbuf }     (* skip blanks *)
  | digit+'.'digit* as lxm  { FLOAT (float_of_string lxm) }
  | ['0'-'9']+ as lxm       { INT (int_of_string lxm) }
  | '<''='                  { LESSEQ}
  | 'i''f'                  { IF }
  | symbol_reg              { create_symbol lexbuf }
  | eof                     { EOF }
  | _  { failwith
	   (Printf.sprintf
	      "don't know how to handle '%s'"
	      (Lexing.lexeme lexbuf))}
