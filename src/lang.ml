(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type variable = Var of string

type binOpExpression = BAdd | BSub | BMult| BDiv | BLEq | BGEq

type exp =
  | ENaN
  | EVar   of variable
  | EInt   of int
  | EFloat of float
  | EBool  of bool
  | EBin   of binOpExpression*exp * exp
  | EIF    of exp*exp*exp
  | ELet   of variable*exp*exp
  | EFun   of variable*exp
  | EApp   of exp*exp

type value = VInt of int | VBool of bool | VFloat of float|
    VFun of variable*exp     | VNaN
        

let string_of_bin_op (op:binOpExpression) : string=
  match op with
    |BAdd  -> "+"
    |BSub  -> "-"
    |BMult -> "*"
    |BDiv  -> "/"      
    |BLEq  -> "<="       
    |BGEq  -> ">="

    
let rec string_of_expression (e:exp): string =
  match e with
    | ENaN     -> "NaN"
    | EVar (Var vname) -> vname
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
    | ELet (Var vname,e1,e2) -> "( let [ " ^vname^" "
      ^ (string_of_expression e1) ^ " ] "
      ^ (string_of_expression e2) ^ " )"
    | EFun (Var vname,e)     -> "( fun ( "^vname^" ) "
      ^ (string_of_expression e) ^ " )"
    | EApp (e1,e2)           -> "( "^ (string_of_expression e1)
      ^" "^(string_of_expression e2) ^" )"

let string_of_value (v:value) : string=
  match v with
    | VNaN     -> "NaN"
    | VInt   n -> string_of_int n
    | VBool  b -> string_of_bool b
    | VFloat f -> string_of_float f
    | VFun (Var vname,e) -> "fun "^ vname^" ->" ^ string_of_expression e 
      
let rec subst (v :value) (var:variable) (e:exp) =
  let val_to_exp v =
    match v with     
    | VNaN     -> ENaN
    | VInt   n -> EInt n
    | VBool  b -> EBool b
    | VFloat f -> EFloat f
    | VFun (v,e) -> EFun (v,e)
  in
  let sub = subst v var in
  let (Var varname) = var in
  match e with
    | ENaN             -> ENaN
    | EInt   n         -> EInt n      
    | EBool  b         -> EBool b
    | EFloat f         -> EFloat f
    | EBin (op,e1, e2) -> EBin (op,
				(sub e1),
				(sub e2))
    | EIF (e1,e2,e3)   -> EIF (sub e1, sub e2, sub e3)
    | ELet (var1,e1, e2) -> sub (subst (eval e1) var1 e2)
    | EApp (EFun(var1,e2), e1)  -> sub (subst (eval e1) var1 e2)
    | EApp ( e1 ,e2)       -> let v = sub e1 |> eval  in
			   begin
			   match v with
			     | VFun (var,e3) ->  sub (subst (eval e3) var e2)
			     | _ -> failwith
			       (Printf.sprintf "Was expecting a function, instead found :%s %s"
				  (string_of_expression e1)
				  (string_of_expression e2))
			   end
    | EVar (Var var1)     -> if var1 = varname
      then (val_to_exp v)
      else EVar (Var var1)
    | EFun (Var var1, e) -> if var1 =varname
      then EFun (Var var1, e)
      else EFun (Var var1, sub e)
	
    
and eval (e:exp) : value =
  match e with
    | ENaN             -> VNaN
    | EInt   n         -> VInt n      
    | EBool  b         -> VBool b
    | EFloat f         -> VFloat f
    | EBin (op,e1, e2) -> eval_bin_op op e1 e2
    | EIF (e1,e2,e3)   ->
      if eval_bool e1
      then eval e2
      else eval e3
    | EFun (var,e)        -> VFun (var,e)
    | ELet (var, e1,e2)   -> eval (subst (eval e1) var e2)
    | EApp (EFun (var,e2),e1) -> eval (subst (eval e1) var e2)
    | EApp (e1 , e2)    -> let v = eval e1 in
			   begin
			   match v with
			     | VFun (var,e3) -> eval (subst (eval e2) var e3)
			     | _ -> failwith
			       (Printf.sprintf "Was expecting a function, instead found :%s %s"
				  (string_of_expression e1)
				  (string_of_expression e2))
			   end
    | EVar (Var var1)     ->failwith
      (Printf.sprintf "Unbound variable :%s"
	 var1)
      
and eval_bin_op (op:binOpExpression) (e1:exp) (e2:exp)=
  let v1 = eval e1 in
  let v2 = eval e2 in
  match (v1,v2) with
    | (VBool _,_) |  ( _,VBool _)  -> failwith
      (Printf.sprintf "Was expecting number, instead found :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
    | (VNaN,_)           -> VNaN
    | (_,VNaN)	         -> VNaN
    | (VInt n1, VInt n2) -> eval_bin_op_int op n1 n2
    | (VInt n1, VFloat f2) -> eval_bin_op_float op (float_of_int n1) f2
    | (VFloat f1, VInt n2) -> eval_bin_op_float op f1 (float_of_int n2)
    | (VFloat f1, VFloat f2) -> eval_bin_op_float op f1 f2
    | _ -> failwith
      (Printf.sprintf "Was expecting number, instead found :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
      
and eval_bin_op_int (op:binOpExpression) (n1:int) (n2:int)=
  match op with
    | BAdd  -> VInt( n1 +  n2)
    | BSub  -> VInt( n1 - n2)
    | BMult -> VInt( n1 * n2)
    | BLEq  -> VBool (n1 <= n2)
    | BGEq  -> VBool (n1 >= n2)       
    | BDiv  -> if n2 ==0 then VNaN else VInt( n1 / n2)

and eval_bin_op_float  (op:binOpExpression) (f1:float) (f2:float)=
  match op with
    | BAdd  -> VFloat( f1 +.  f2)
    | BSub  -> VFloat( f1 -. f2)
    | BMult -> VFloat( f1 *. f2)
    | BLEq  -> VBool (f1 <= f2)
    | BGEq  -> VBool (f1 >= f2)
    | BDiv  -> if f2 ==0.0 then VNaN else VFloat( f1 /. f2)
	
and eval_bool (e:exp) : bool=
    let v = eval e in
  match v with
    |  VBool b -> b
    |  _ -> failwith (Printf.sprintf
			"Type error, was expecting bool instead got: %s from %s"
			(string_of_value v) (string_of_expression e))
