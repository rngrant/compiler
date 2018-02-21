(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/parser.mly *)
%token <int> INT
%token <float> FLOAT
%token NAN
%token LPAREN RPAREN
%token IF THEN ELSE
%token PLUS MINUS TIMES DIV LESSEQ
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left LESSEQ            /* highest precedence */
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
  : n = INT                { EInt n }
| f = FLOAT                { EFloat f}
| NAN                      { ENaN }
| LPAREN  e1=expr RPAREN   { e1 }
| e1=expr PLUS   e2=expr   { EBin (BAdd ,e1,  e2) }
| e1=expr MINUS  e2=expr   { EBin (BSub , e1, e2) }
| e1=expr TIMES  e2=expr   { EBin (BMult, e1, e2) }
| e1=expr DIV    e2=expr   { EBin (BDiv, e1, e2) }    
| e1=expr LESSEQ e2=expr   { EBin (BLEq, e1, e2) }
| IF   e1=expr THEN e2=expr ELSE e3=expr  { EIF (e1, e2,e3) }
;
