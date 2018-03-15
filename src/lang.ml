(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type typ  = TNaN| TInt | TFloat| TBool | TUnit
	    |TArrow of typ*typ |TPair of typ*typ

type variable = Var of string

type ctx   =  (variable*typ) list

type uniOpExpression  = UFst| USnd
    
type binOpExpression = BAdd | BSub | BMult| BDiv | BLEq | BGEq | BGT| BLT| BEq

type boolOp   = BAnd| BOr

type exp =
  | ENaN
  | EUnit
  | EVar      of variable
  | EInt      of int
  | EFloat    of float
  | EBool     of bool
  | EPair     of exp*exp
  | EUni      of uniOpExpression*exp
  | EBin      of binOpExpression*exp * exp
  | EBinBool  of boolOp*exp * exp
  | EIF       of exp*exp*exp
  | ELet      of typ*variable*exp*exp
  | EFun      of typ*typ*variable*exp
  | EFix      of typ*typ*variable*variable*exp
  | EApp      of exp*exp

type value = VInt of int | VBool of bool | VFloat of float|
    VFun of typ*typ*variable*exp | VFix of typ*typ*variable*variable*exp
	     |VNaN | VUnit | VPair of exp*exp
        

let string_of_uni_op (op:uniOpExpression) : string=
  match op with
    |UFst  -> "fst"
    |USnd  -> "snd"
	
let string_of_bin_op (op:binOpExpression) : string=
  match op with
    |BAdd  -> "+"
    |BSub  -> "-"
    |BMult -> "*"
    |BDiv  -> "/"      
    |BLEq  -> "<="       
    |BGEq  -> ">="
    |BGT   -> ">"
    |BLT   -> "<"
    |BEq   -> "="

let string_of_bool_op (op:boolOp): string =
  match op with
    |BAnd  -> "and"
    |BOr   -> "or"

    
let rec string_of_expression (e:exp): string =
  match e with
    | ENaN     -> "NaN"
    | EUnit    -> "()"
    | EVar (Var vname) -> vname
    | EInt  n  -> string_of_int  n
    | EBool b  -> string_of_bool b
    | EFloat f -> string_of_float f
    | EPair (e1,e2) -> "( pair "
      ^(string_of_expression e1) ^" "
      ^(string_of_expression e2)^ " )"
    | EIF (e1,e2,e3)  -> "( if "
      ^(string_of_expression e1) ^" "
      ^(string_of_expression e2)^ " "
      ^ (string_of_expression e3) ^ " )"
    | EBin (op,e1,e2) -> "( "
      ^ (string_of_bin_op op) ^ " "
      ^ (string_of_expression e1)^ " "
      ^ (string_of_expression e2) ^" )"
    | EUni (op,e)    -> "( "
      ^ (string_of_uni_op op) ^" "
      ^ (string_of_expression e)^" )"
    | EBinBool (op,e1,e2) ->"( "
      ^ (string_of_bool_op op) ^ " "
      ^ (string_of_expression e1)^ " "
      ^ (string_of_expression e2) ^" )"
    | ELet (_,Var vname,e1,e2) -> "( let [ " ^vname^" "
      ^ (string_of_expression e1) ^ " ] "
      ^ (string_of_expression e2) ^ " )"
    | EFun (_,_,Var vname,e)     -> "( fun ( "^vname^" ) "
      ^ (string_of_expression e) ^ " )"
    |  EFix (_,_,Var vname1,Var vname2,e) ->"( fix ( "^vname1^" , "^vname2^" ) "
      ^ (string_of_expression e) ^ " )"
    | EApp (e1,e2)           -> "( "^ (string_of_expression e1)
      ^" "^(string_of_expression e2) ^" )"

let string_of_value (v:value) : string=
  match v with
    | VNaN     -> "NaN"
    | VUnit    -> "()"
    | VInt   n -> string_of_int n
    | VBool  b -> string_of_bool b
    | VFloat f -> string_of_float f
    | VPair (e1,e2) -> "( "
      ^(string_of_expression e1)^" , "
      ^(string_of_expression e2)^" )"
    | VFun (_,_,Var vname,e) -> "fun "^ vname^" ->" ^ string_of_expression e
    | VFix (_,_,Var vname1,Var vname2,e) ->
      "fix "^ vname1^" "^vname2^" ->" ^ string_of_expression e

let rec string_of_type (t:typ) : string=
  match t with
    | TNaN         -> "NaN"
    | TUnit        -> "()"
    | TInt         -> "int"
    | TFloat       -> "float"
    | TBool        -> "bool"
    | TPair (t1,t2) -> (string_of_type t1) ^ " * " ^ (string_of_type t2)
    | TArrow (t1,t2) -> (string_of_type t1) ^ "->" ^ (string_of_type t2)

let val_to_exp v =
    match v with     
      | VNaN     -> ENaN
      | VUnit    -> EUnit
      | VInt   n -> EInt n
      | VBool  b -> EBool b
      | VFloat f -> EFloat f
      | VPair (e1,e2) -> EPair (e1,e2)
      | VFun (t1,t2,v,e) -> EFun (t1,t2,v,e)
      | VFix(t1,t2,v1,v2,e) -> EFix(t1,t2,v1,v2,e)

let exp_to_value e =
    match e with     
    | ENaN     -> VNaN
    | EInt   n -> VInt n
    | EBool  b -> VBool b
    | EFloat f -> VFloat f
    | EFun (t1,t2,v,e) -> VFun (t1,t2,v,e)
    | EFix(t1,t2,v1,v2,e) -> VFix(t1,t2,v1,v2,e)
    | _             ->failwith
      (Printf.sprintf "Expected value instead found : %s"
	 (string_of_expression e))

let rec is_value (e:exp) =
  match e with
    | ENaN |EUnit |EInt _ | EBool _ | EFloat _ | EFun _| EFix _ -> true
    |  EPair (e1,e2) -> is_value e1 && is_value e2
    | _ -> false      

      
let rec subst (v :value) (var:variable) (e:exp) =
  let sub = subst v var in
  let (Var varname) = var in
  match e with
    | ENaN             -> ENaN
    | EUnit            -> EUnit
    | EInt   n         -> EInt n      
    | EBool  b         -> EBool b
    | EFloat f         -> EFloat f
    | EPair (e1,e2)    -> EPair(sub e1, sub e2)
    | EUni (op,e)      -> EUni (op, sub e)
    | EBin (op,e1, e2) -> EBin (op,
				(sub e1),
				(sub e2))
    | EBinBool (op,e1, e2) -> EBinBool (op,
				(sub e1),
				(sub e2))
    | EIF (e1,e2,e3)   -> EIF (sub e1, sub e2, sub e3)
    | ELet (t,var1,e1, e2) -> ELet (t,var1, (sub e1), (sub e2))
    | EApp ( e1 ,e2)       -> EApp (sub e1 ,sub e2)
    | EVar (Var var1)     -> if var1 = varname
      then (val_to_exp v)
      else EVar (Var var1)
    | EFun (t1,t2,Var var1, e) -> if var1 =varname
      then EFun (t1,t2,Var var1, e)
      else EFun (t1,t2,Var var1, sub e)
    | EFix(t1,t2,Var var1,Var var2,e) ->
      if var1 = varname || var2 =varname then
	EFix(t1,t2,Var var1,Var var2,e)
      else
	EFix(t1,t2,Var var1,Var var2,sub e)
	
    
let rec eval (e:exp) : value =
    if (is_value e)
    then exp_to_value e
    else step e |> eval

and eval_bool_op (op:boolOp) (e1:exp) (e2:exp)=
  let v1 = eval e1 in
  let v2 = eval e2 in
  match (op,v1,v2) with
    | (BAnd,VBool v1, VBool v2)  -> VBool (v1 && v2)
    | (BOr,VBool v1, VBool v2)   -> VBool (v1 || v2)
    | _ -> failwith
      (Printf.sprintf "Was expecting a Boolean, instead found :(%s %s %s)"
	 (string_of_bool_op op)
	 (string_of_value v1)
	 (string_of_value v2))

and eval_uni_op (op:uniOpExpression) (e:exp) =
  let v = eval e in
  match (op,v) with
    | (UFst, VPair (v1,v2)) -> eval v1
    | (USnd, VPair (v1,v2)) -> eval v2
    | (UFst,_) | (USnd,_) -> failwith
      (Printf.sprintf "Was expecting Pair, instead found :(%s %s)"
	 (string_of_uni_op op)
	 (string_of_value v))
      
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
    | BLT  ->  VBool (n1 < n2)
    | BGT  ->  VBool (n1 > n2)
    | BEq ->   VBool (n1 = n2)             
    | BDiv  -> if n2 ==0 then VNaN else VInt( n1 / n2)

and eval_bin_op_float  (op:binOpExpression) (f1:float) (f2:float)=
  match op with
    | BAdd  -> VFloat( f1 +.  f2)
    | BSub  -> VFloat( f1 -. f2)
    | BMult -> VFloat( f1 *. f2)
    | BLEq  -> VBool (f1 <= f2)
    | BGEq  -> VBool (f1 >= f2)
    | BLT  ->  VBool (f1 < f2)
    | BGT  ->  VBool (f1 > f2)
    | BEq ->   VBool (f1 = f2)   
    | BDiv  -> if f2 ==0.0 then VNaN else VFloat( f1 /. f2)
	
and eval_bool (e:exp) : bool=
    let v = eval e in
  match v with
    |  VBool b -> b
    |  _ -> failwith (Printf.sprintf
			"Type error, was expecting bool instead got: %s from %s"
			(string_of_value v) (string_of_expression e))

and step (e:exp) : exp =
  match e with
    | ENaN             -> ENaN
    | EUnit            -> EUnit
    | EInt   n         -> EInt n      
    | EBool  b         -> EBool b
    | EFloat f         -> EFloat f
    | EPair (e1,e2)    -> step_pair e1 e2
    | EFun (t1,t2,var,e)        -> EFun (t1,t2,var,e)
    | EFix (t1,t2,var1,var2,e)  -> EFix (t1,t2,var1,var2,e)
    | EUni (op,e)      -> step_uni_op op e
    | EBin (op,e1, e2) -> step_bin_op op e1 e2      
    | EBinBool (op,e1, e2) -> step_bool_op op e1 e2
    | EIF (e1,e2,e3)   -> step_if e1 e2 e3
    | ELet (t,var, e1,e2) -> if is_value e1
      then subst (exp_to_value e1) var e2
      else  ELet (t,var,step e1,e2)
    | EApp (e1 , e2)    -> step_app e1 e2
    | EVar (Var var1)     ->failwith
      (Printf.sprintf "Unbound variable :%s"
	 var1)

and step_pair e1 e2 =
  if is_value e1 then
    begin
      if is_value e2 then EPair (e1,e2)
      else EPair(e1,step e2)
    end
  else EPair(step e1,e2)

and step_uni_op op e =
  if is_value e then
    val_to_exp (eval_uni_op op e)
  else EUni (op, step e)
    
and step_bin_op op e1 e2 =
  if is_value e1 then
    begin
      if is_value e2 then val_to_exp (eval_bin_op op e1 e2)
      else EBin (op, e1, step e2)
    end
  else EBin (op,step e1, e2)
      
and step_bool_op op e1 e2 =
  if is_value e1 then
    begin
      if is_value e2 then val_to_exp (eval_bool_op op e1 e2)
      else EBinBool (op, e1, step e2)
    end
  else EBinBool (op,step e1, e2)

and step_if e1 e2 e3 =
  if is_value e1 then
    let v = exp_to_value e1
    in
    begin
      match v with
	| VBool true -> e2
	| VBool false -> e3
	|  _ -> failwith
	  (Printf.sprintf
	     "Type error, was expecting bool instead got: %s from %s"
	     (string_of_value v) (string_of_expression e1))
    end
  else EIF (step e1,e2,e3)

and step_app e1 e2 =
  if is_value e1 then
    if is_value e2 then
      begin
      match e1 with
	| EFun (_,_,var,e3) -> subst (exp_to_value e2) var e3
	| EFix (_,_,var1,var2,e3) as vF ->
	  subst (exp_to_value vF) var1 (subst (exp_to_value e2) var2 e3)
	| _ -> failwith
	  (Printf.sprintf "Was expecting a function, instead found :%s %s"
	     (string_of_expression e1)
	     (string_of_expression e2))
      end
    else  EApp ( e1, step e2)
  else
    EApp ( step e1, e2)

  
let rec typecheck (c:ctx) (e:exp) : typ =
  let check_context v c = List.assoc v c 
  in
  match e with
    | ENaN   -> TNaN
    | EUnit   -> TUnit
    | EVar v ->  check_context v c
    | EInt _ -> TInt
    | EFloat _ -> TFloat
    | EBool _  -> TBool
    | EPair(e1,e2) -> TPair(typecheck c e1,typecheck c e2)
    | EUni  (op,e) -> typecheck_uni_op c op e
    | EBin  (op,e1, e2) -> typecheck_bin_op c op e1 e2 
    | EBinBool (op,e1, e2) -> typecheck_bool_op c op e1 e2 
    | EIF (e1,e2,e3)      -> typecheck_if c e1 e2 e3     
    | ELet (expected_t,var, e1,e2) ->
      let actual_t= typecheck c e1 in
      if expected_t = actual_t then
	typecheck ((var,actual_t)::c) e2
      else failwith
	(Printf.sprintf "Was expecting a : %s, instead found : %s in\n %s"
	     (string_of_type expected_t)
	     (string_of_type actual_t)
	     (string_of_expression e2))
    | EFun (var_t,expected_t,var, e) ->
      let actual_t = typecheck ((var,var_t)::c) e
      in
      if expected_t = actual_t then
	TArrow (var_t, actual_t)
      else failwith
	(Printf.sprintf "Was expecting a : %s, instead found : %s in\n %s"
	     (string_of_type expected_t)
	     (string_of_type actual_t)
	     (string_of_expression e))
    | EFix (var_t,expected_t,var1,var2,e)  ->
      let actual_t=
	typecheck ((var1,(TArrow (var_t,expected_t)))::((var2,var_t)::c)) e
      in
      if expected_t = actual_t then
	TArrow (var_t, actual_t)
      else failwith
	(Printf.sprintf "Was expecting a : %s, instead found : %s in\n %s"
	     (string_of_type expected_t)
	     (string_of_type actual_t)
	     (string_of_expression e))
    | EApp (e1,e2) -> typecheck_app c e1 e2

and typecheck_uni_op (c:ctx) (op:uniOpExpression) (e:exp) : typ =
  let t = typecheck c e in
  match (op,t) with
    | (UFst, TPair (t1,t2)) -> t1
    | (USnd, TPair (t1,t2)) -> t2
    | (UFst, _)| (USnd, _) -> failwith
       (Printf.sprintf "%s must be applied to a pair, instead found : %s in\n %s"
	  (string_of_uni_op op)
	  (string_of_type t)
	  (string_of_expression e))
      
and typecheck_bin_op (c:ctx) (op:binOpExpression) (e1:exp) (e2:exp) : typ =
  let t1 = typecheck c e1 in
  let t2 = typecheck c e2 in
  let ret_typ = begin
    match op with
      | BAdd | BSub | BMult| BDiv -> t1
      | BLEq | BGEq | BGT| BLT| BEq -> TBool
  end
  in
  match (t1, t2) with
    | (TInt, TInt)     -> ret_typ
    | (TFloat, TFloat) -> ret_typ
    | (TNaN,_) | (_,TNaN) -> if ret_typ =TBool then TBool else TNaN
    | _ -> failwith
      (Printf.sprintf
	 "Was expecting compatible numerical types, instead found %s and %s in\n %s"
	 (string_of_type t1)
	 (string_of_type t2)
	     (string_of_expression (EBin(op,e1,e2))))
  
and typecheck_bool_op (c:ctx) (op:boolOp) (e1:exp) (e2:exp) : typ =
  let t1 = typecheck c e1 in
  let t2 = typecheck c e2 in
  match (t1, t2) with
    | (TBool, TBool)     -> TBool
    | _ -> failwith
      (Printf.sprintf
	 "Was expecting Boolean values, instead found %s and %s in\n %s"
	 (string_of_type t1)
	 (string_of_type t2)
	 (string_of_expression (EBinBool(op,e1,e2))))
  
and typecheck_if  (c:ctx) (e1:exp) (e2:exp) (e3:exp) :typ =
   let t1 = typecheck c e1 in
   let t2 = typecheck c e2 in
   let t3 = typecheck c e2 in
   match t1 with
     | TBool ->
       begin if t2=t3 then t2 else
	 failwith
      (Printf.sprintf
	 "If statement returns different types %s and %s in\n %s"
	 (string_of_type t2)
	 (string_of_type t3)
	 (string_of_expression (EIF(e1,e2,e3))))
     end
     | _ -> failwith
      (Printf.sprintf
	 "If statement has non boolean conditional in\n %s"
	 (string_of_expression (EIF(e1,e2,e3))))
		       
and typecheck_app (c:ctx) (e1:exp) (e2:exp): typ =
  let t1 = typecheck c e1 in
  let t2 = typecheck c e2 in
  match (t1,t2) with
    | ((TArrow (arg, ret)),input) ->
      if arg = input then ret
      else failwith
      (Printf.sprintf
	 "Type mismatch: Expected %s , found %s in\n %s"
	 (string_of_type arg)
	 (string_of_type input)
	 (string_of_expression (EApp(e1,e2))))
    | _                       ->  failwith (Printf.sprintf
	 "Attempted to apply non function in\n %s"	 
	 (string_of_expression (EApp(e1,e2))))
