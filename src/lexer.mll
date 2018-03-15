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

let token_from_symbol symbol =
  match symbol with
    |'('      -> LPAREN
    |')'      -> RPAREN
    |'+'      -> PLUS
    |'-'      -> MINUS
    |'*'      -> TIMES
    |'/'      -> DIV
    |'='      -> EQUAL
    |'>'      -> GTHAN
    | ':'     -> COLON
    | ','     -> COMMA
    |'<'      -> LTHAN
    | _       -> failwith
      (Printf.sprintf "Expecting symbol, instead found :%c"
	 symbol)


let token_from_word word =
  match word with
    | "if"   -> IF
    | "NaN"  -> NAN
    | "then" -> THEN
    | "else" -> ELSE
    | "let"  -> LET
    | "in"   -> IN
    | "fun"  -> FUN
    | "fix"  -> FIX
    | "true" -> BOOL true
    | "false" -> BOOL false
    | "and"   -> AND
    | "or"    -> OR
    | "fst"   -> FST
    | "snd"   -> SND
    | "int"   -> TINT
    | "float" -> TFLOAT
    | "bool"  -> TBOOL
    | _      -> VARIABLE (word)


  let token_from_symbols symbols =
  match symbols with
    | "<="   -> LESSEQ
    | ">="   -> GREATEQ
    | "->"   -> RARROW
    | "()"   -> UNIT
    | _      ->  failwith
      (Printf.sprintf
	 "Expecting symbols, instead found :%s"
	 symbols) 
    
let create_int lexbuf = lexeme lexbuf |> int_of_string
}

let newline    = '\n' | ('\r' '\n') | '\r'
let whitespace = ['\t' ' ']
let digit      = ['0'-'9']
let single_char_symbol =
  '(' | ')' | ':'| '+' | '-' | '/' | '=' |'*'|','| '<' | '>'
let multi_char_symbol =  '-' | '<' | '=' | '>'
let character = ['a'-'z']|['A'-'Z']
let variable_characters = character | '_'| digit
    
rule token = parse
  | whitespace+ | newline+    { token lexbuf }     (* skip blanks *)
  | digit+'.'digit* as lxm    { FLOAT (float_of_string lxm) }
  | ['0'-'9']+ as lxm         { INT (int_of_string lxm) }
  | variable_characters+ as lxm {token_from_word lxm }
  | single_char_symbol as lxm { token_from_symbol lxm }      
  | multi_char_symbol+ as lxm { token_from_symbols lxm }
  | eof                       { EOF }
  | _  { failwith
	   (Printf.sprintf
	      "don't know how to handle '%s'"
	      (Lexing.lexeme lexbuf))}
