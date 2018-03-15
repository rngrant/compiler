
type typ     = | TNaN| TInt | TFloat| TBool | TUnit
	       | TArrow of typ*typ|TPair of typ*typ
    
type variable = | Var of string

type ctx  = (variable*typ) list

type uniOpExpression  = | UFst| USnd
    
type binOpExpression  = | BAdd | BSub | BMult| BDiv | BLEq | BGEq | BGT| BLT| BEq

type boolOp   =  | BAnd| BOr 

    
type exp =
  | ENaN
  | EUnit
  | EVar   of variable
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EPair     of exp*exp
  | EUni      of uniOpExpression*exp
  | EBin   of binOpExpression*exp * exp      
  | EBinBool  of boolOp*exp * exp
  | EIF    of exp*exp*exp
  | ELet   of typ*variable*exp*exp
  | EFun      of typ*typ*variable*exp
  | EFix      of typ*typ*variable*variable*exp
  | EApp      of exp*exp


type value =  | VInt of int | VBool of bool | VFloat of float|
    VFun of typ*typ*variable*exp | VFix of typ*typ*variable*variable*exp
	      |VNaN | VUnit | VPair of exp*exp
        
	
val string_of_expression: exp -> string

val string_of_value: value -> string

val string_of_type: typ -> string
  
val eval: exp -> value

val exp_to_value : exp -> value

val is_value : exp -> bool
  
val step : exp -> exp

val typecheck : ctx -> exp -> typ
