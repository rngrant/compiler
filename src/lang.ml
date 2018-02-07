(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type value = VInt of int | VBool of bool | VFloat of float


let string_of_bool b = if b then "true" else "false"
    
let string_of_value (v:value) : string=
  match v with
    | VInt   n -> string_of_int n
    | VBool  b -> string_of_bool b
    | VFloat f -> string_of_float f

type binOpExpression = BAdd | BSub | BMult| BDiv | BLEq
    
type exp =
  | EInt of int
  | EFloat of float
  | EBool of bool
  | EBin of binOpExpression*exp * exp
  | EIF  of exp*exp*exp



let string_of_bin_op (op:binOpExpression) : string=
  match op with
    |BAdd  -> "+"
    |BSub  -> "-"
    |BMult -> "*"
    |BDiv  -> "/"      
    |BLEq  -> "<="

    
let rec string_of_expression (e:exp): string =
  match e with
    | EInt  n  -> string_of_int  n
    | EBool b  -> string_of_bool b
    | EFloat f -> string_of_float f
    | EIF (e1,e2,e3)  -> "( if "
      ^(string_of_expression e1) ^" "
      ^(string_of_expression e2)^ " "
      ^ (string_of_expression e3) ^ " )"
    | EBin (op,e1,e2) -> "( "
      ^ (string_of_bin_op op) ^ " "
      ^ (string_of_expression e1)^ " "
      ^ (string_of_expression e2) ^" )"
    
let rec interpret (e:exp) : value =
  match e with
    | EInt   n         -> VInt n
    | EBool  b         -> VBool b
    | EFloat f         -> VFloat f
    | EBin (op,e1, e2) -> interpret_bin_op op e1 e2
    | EIF (e1,e2,e3)   -> if interpret_bool e1 then interpret e2 else interpret e3
      
and interpret_bin_op (op:binOpExpression) (e1:exp) (e2:exp)=
  let v1 = interpret e1 in
  let v2 = interpret e2 in
  match (v1,v2) with
    | (VInt n1, VInt n2) -> interpret_bin_op_int op n1 n2
    | (VInt n1, VFloat f2) -> interpret_bin_op_float op (float_of_int n1) f2
    | (VFloat f1, VInt n2) -> interpret_bin_op_float op f1 (float_of_int n2)
    | (VFloat f1, VFloat f2) -> interpret_bin_op_float op f1 f2
    | (v1        ,v2     )    -> failwith
      (Printf.sprintf "Was expecting number, instead found :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
      
and interpret_bin_op_int (op:binOpExpression) (n1:int) (n2:int)=
  match op with
    | BAdd  -> VInt( n1 +  n2)
    | BSub  -> VInt( n1 - n2)
    | BMult -> VInt( n1 * n2)
    | BLEq  -> VBool (n1 <= n2) 
    | BDiv  -> if n2 ==0 then
		 failwith
		   (Printf.sprintf "Division by zero:(/ %d %d)"
		      n1
		      n2)
	       else VInt( n1 / n2)

and interpret_bin_op_float  (op:binOpExpression) (f1:float) (f2:float)=
  match op with
    | BAdd  -> VFloat( f1 +.  f2)
    | BSub  -> VFloat( f1 -. f2)
    | BMult -> VFloat( f1 *. f2)
    | BLEq  -> VBool (f1 <= f2) 
    | BDiv  -> if f2 ==0.0 then
		 failwith
		   (Printf.sprintf "Division by zero:(/ %f %f)"
		      f1
		      f2)
	       else VFloat( f1 /. f2)
and interpret_bool (e:exp) : bool=
    let v = interpret e in
  match v with
    |  VBool b -> b
    |  _ -> failwith (Printf.sprintf
			"Type error, was expecting bool instead got: %s from %s"
			(string_of_value v) (string_of_expression e))
