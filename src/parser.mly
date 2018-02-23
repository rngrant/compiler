(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/parser.mly *)
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VARIABLE

%token NAN
%token FUN RARROW
%token LPAREN RPAREN
%token LET  EQUAL IN
%token IF THEN ELSE
%token AND OR
%token PLUS MINUS TIMES DIV LESSEQ GREATEQ GTHAN LTHAN
%left IN  RARROW     /* lowest precedence */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left LESSEQ  GREATEQ   /* highest precedence */
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
| b = BOOL                 { EBool b}
| v = VARIABLE             { EVar (Var( v) )}
| NAN                      { ENaN }    
| LPAREN  e1=expr RPAREN   { e1 }
| LET v= VARIABLE EQUAL e1=expr IN e2=expr   { ELet (Var (v),e1,e2) }
| FUN v= VARIABLE RARROW e=expr   { EFun (Var(v),e) }
| e1=expr PLUS   e2=expr   { EBin (BAdd ,e1,  e2) }
| e1=expr MINUS  e2=expr   { EBin (BSub , e1, e2) }
| e1=expr TIMES  e2=expr   { EBin (BMult, e1, e2) }
| e1=expr DIV    e2=expr   { EBin (BDiv, e1, e2) }    
| e1=expr LESSEQ e2=expr   { EBin (BLEq, e1, e2) }
| e1=expr GREATEQ e2=expr  { EBin (BGEq, e1, e2) }
| e1=expr GTHAN e2=expr  { EBin (BGT, e1, e2) }
| e1=expr LTHAN e2=expr  { EBin (BLT, e1, e2) }
| e1=expr EQUAL e2=expr  { EBin (BEq, e1, e2) }
| e1=expr AND e2=expr  { EBinBool (BAnd, e1, e2) }
| e1=expr OR e2=expr  { EBinBool (BOr, e1, e2) }
| IF e1=expr THEN e2=expr ELSE e3=expr  { EIF (e1, e2,e3) }    
| e1= expr e2=expr  { EApp (e1,e2) }
;
