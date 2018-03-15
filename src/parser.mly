(*Starting code taken from 
 * https://github.com/ocaml/ocamlbuild/blob/master/...
 * examples/05-lex-yacc/src/parser.mly *)
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> VARIABLE
%token TINT TFLOAT TBOOL
%token FIX
%token COLON COMMA APPEND
%token FST SND
%token HEAD TAIL EMPTY
%token LBRACE RBRACE
%token NAN UNIT
%token FUN RARROW
%token LPAREN RPAREN
%token LET  EQUAL IN
%token IF THEN ELSE
%token AND OR
%token PLUS MINUS TIMES DIV LESSEQ GREATEQ GTHAN LTHAN

%left Application
%right RARROW
%right APPEND
%left AND OR
%left IN       /* lowest precedence */
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%left LESSEQ  GREATEQ  GTHAN LTHAN FST SND  /* highest precedence */

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
  : LPAREN  e1=expr RPAREN   { e1 }
| LET v= VARIABLE COLON t=ttype
    EQUAL e1=expr IN e2=expr
    { ELet (t,Var (v),e1,e2) }
| FIX v1= VARIABLE  LPAREN v2 = VARIABLE COLON t1=ttype RPAREN 
     COLON t2=ttype EQUAL  e=expr
    {EFix (t1,t2,Var (v1), Var(v2), e)}
| FUN LPAREN v=VARIABLE COLON t1=ttype RPAREN COLON t2=ttype EQUAL e=expr
    { EFun (t1,t2,Var(v),e) }
| LPAREN  e1=expr COMMA e2=expr RPAREN   { EPair(e1,e2) }
| FST e=expr               { EUni (UFst, e)}
| SND e=expr               { EUni (USnd, e)}
| EMPTY e=expr               { EUni (UEmpty, e)}
| HEAD e=expr               { EUni (UHead, e)}
| TAIL e=expr               { EUni (UTail, e)}
| e1=expr PLUS   e2=expr   { EBin (BAdd ,e1,  e2) }
| e1=expr MINUS  e2=expr   { EBin (BSub , e1, e2) }
| e1=expr TIMES  e2=expr   { EBin (BMult, e1, e2) }
| e1=expr DIV    e2=expr   { EBin (BDiv, e1, e2) }    
| e1=expr LESSEQ e2=expr   { EBin (BLEq, e1, e2) }
| e1=expr GREATEQ e2=expr  { EBin (BGEq, e1, e2) }
| e1=expr GTHAN e2=expr    { EBin (BGT, e1, e2) }
| e1=expr LTHAN e2=expr    { EBin (BLT, e1, e2) }
| e1=expr EQUAL e2=expr    { EBin (BEq, e1, e2) }
| e1=expr AND e2=expr      { EBinBool (BAnd, e1, e2) }
| e1=expr OR e2=expr       { EBinBool (BOr, e1, e2) }
| IF e1=expr THEN e2=expr ELSE e3=expr  { EIF (e1, e2,e3) }    
| e1= expr e2=expr  { EApp (e1,e2) } %prec Application
| e = expLit      {e}
| e = expList      {e}
;

(*
binOp
:  PLUS      {BAdd}
| MINUS      {BSub}
| TIMES      {BMult}
| DIV        {BDiv}
| LESSEQ     {BLEq}
| GREATEQ    {BGEq}
| GTHAN      {BGT}
| LTHAN      {BLT}
| EQUAL      {BEq}
;

binBoolOp
 : AND {BAnd}
|OR {BOr}
;*)


expList
  :LBRACE RBRACE  COLON t=ttype          { EEmptyList t}
| LPAREN e=expList RPAREN          {e}
| e1=expr APPEND e2=expList        {ECons(e1,e2)}


expLit
  : n = INT                { EInt n }
| f = FLOAT                { EFloat f}
| b = BOOL                 { EBool b}
| v = VARIABLE             { EVar (Var( v) )}
| NAN                      { ENaN }
| UNIT                      { EUnit }
| LPAREN RPAREN             { EUnit }
;

ttype
  : TINT                     { TInt }
| TFLOAT                     { TFloat}
| TBOOL                      { TBool}
| LPAREN RPAREN              { TUnit}
| NAN                        { TNaN}
| t1=ttype RARROW t2 = ttype  { TArrow(t1,t2)}
| t1=ttype TIMES t2 = ttype  { TPair(t1,t2)}
| LBRACE t=ttype RBRACE      { TList(t) }
;

