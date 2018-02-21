(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/parser.mly *)
%token <int> INT
%token LPAREN RPAREN
%token PLUS MINUS TIMES DIV
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%start main             /* the entry point */
%token EOF

%{
  open Lang
%}


%type <Lang.exp> main

%%

(* Menhir let us give names to symbol values,
   instead of having to use $1, $2, $3 as in ocamlyacc *)
main
  : e = expr EOF                { e }
  ;

expr
  : n = INT                 { EInt n }
  | LPAREN PLUS  e1=expr  e2=expr RPAREN   { EBin (BAdd ,e1,  e2) }
  | LPAREN MINUS e1=expr e2=expr RPAREN  { EBin (BSub , e1, e2) }
  | LPAREN TIMES e1=expr  e2=expr RPAREN { EBin (BMult, e1, e2) }
  | LPAREN DIV   e1=expr e2=expr RPAREN  { EBin (BDiv, e1, e2) }
  ;
