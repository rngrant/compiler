type variable = | Var of string

type binOpExpression  = | BAdd | BSub | BMult| BDiv | BLEq | BGEq | BGT| BLT| BEq

type boolOp   =  | BAnd| BOr 
    
type exp =
  | ENaN
  | EVar   of variable
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EBin   of binOpExpression*exp * exp      
  | EBinBool  of boolOp*exp * exp
  | EIF    of exp*exp*exp
  | ELet   of variable*exp*exp
  | EFun   of variable*exp
  | EApp   of exp*exp


type value =  | VInt of int | VBool of bool | VFloat of float|
    VFun of variable*exp     | VNaN


val string_of_expression: exp -> string

val eval: exp -> value

val string_of_value: value -> string
