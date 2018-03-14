
type typ     = | TInt | TFloat| TBool | TArrow of typ*typ
    
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
  | ELet   of typ*variable*exp*exp
  | EFun      of typ*typ*variable*exp
  | EFix      of typ*typ*variable*variable*exp
  | EApp      of exp*exp


type value =  | VInt of int | VBool of bool | VFloat of float|
    VFun of typ*typ*variable*exp | VFix of typ*typ*variable*variable*exp |VNaN
        
	
val string_of_expression: exp -> string

val string_of_value: value -> string

val eval: exp -> value

val exp_to_value : exp -> value

val is_value : exp -> bool
  
val step : exp -> exp
