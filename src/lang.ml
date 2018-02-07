(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type value = VInt of int | VBool of bool


let string_of_bool b = if b then "true" else "false"
    
let string_of_value (v:value) : string=
  match v with
    | VInt n -> string_of_int n
    | VBool b -> string_of_bool b

type binOpExpression = BAdd | BSub | BMult| BDiv | BLEq
    
type exp =
  | EInt of int
  | EBool of bool
  | EBin of binOpExpression*exp * exp



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
    | EBin (op,e1,e2) -> "( "
      ^ (string_of_bin_op op) ^ " "
      ^ (string_of_expression e1)^ " "
      ^ (string_of_expression e2) ^" )"
    
let rec interpret (e:exp) : value =
  match e with
    | EInt n        -> VInt n
    | EBool b        -> VBool b
    | EBin (op,e1, e2) ->interpret_bin_op op e1 e2
      
and interpret_bin_op (op:binOpExpression) (e1:exp) (e2:exp)=
  match op with
    | BAdd  -> VInt( interpret_int e1 + interpret_int e2)
    | BSub  -> VInt( interpret_int e1 - interpret_int e2)
    | BMult -> VInt( interpret_int e1 * interpret_int e2)
    | BLEq  -> VBool (interpret_int e1 <= interpret_int e2) 
    | BDiv  -> let div = interpret_int e2 in
	       if div ==0 then
		 failwith
		   (Printf.sprintf "Division by zero:(/ %s %s)"
		      (string_of_expression e1)
		      (string_of_expression e2))
	       else VInt( interpret_int e1 / interpret_int e2)
and interpret_int (e:exp) : int =
  let v = interpret e in
  match v with
    | VInt n  -> n
    | _ -> failwith (Printf.sprintf "Type error, was expecting int instead got: %s" (string_of_value v))
and interpret_bool (e:exp) : bool=
    let v = interpret e in
  match v with
    |  VBool b -> b
    |  _ -> failwith (Printf.sprintf "Type error, was expecting bool instead got: %s"  (string_of_value v))
