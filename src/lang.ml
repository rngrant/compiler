(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type typ  = TNaN| TInt | TFloat| TBool | TUnit
	    |TArrow of typ*typ |TTuple of int*(typ list) | TList of typ | TRef of typ

type variable = Var of string

type ctx   =  (variable*typ) list

type uniOpExpression  = UFst| USnd | UTail| UHead | UEmpty | UBang | URef
			| UNth of int
    
type binOpExpression = BAdd | BSub | BMult| BDiv | BLEq | BGEq
		       | BGT| BLT| BEq | BSeq |BSetEq 

type boolOp   = BAnd| BOr

type exp =
  | ENaN
  | EUnit
  | EPtr       of int
  | EEmptyList of typ
  | ECons      of exp*exp
  | EVar       of variable
  | EInt       of int
  | EFloat     of float
  | EBool      of bool
  | ETuple     of int*(exp list)
  | EUni       of uniOpExpression*exp
  | EBin       of binOpExpression*exp * exp      
  | EBinBool   of boolOp*exp * exp
  | EIF        of exp*exp*exp
  | ELet       of typ*variable*exp*exp
  | EFun       of typ*typ*variable*exp
  | EFix       of typ*typ*variable*variable*exp
  | EWhile     of exp*exp
  | EApp       of exp*exp

type value = VInt of int | VBool of bool | VFloat of float| VPtr of int|
    VFun of typ*typ*variable*exp | VFix of typ*typ*variable*variable*exp
	     | VNaN | VUnit | VTuple of int*(exp list)
	     | VCons of exp*exp | VEmptyList of typ

type environment   = (int*value) list		 

let string_of_uni_op (op:uniOpExpression) : string=
  match op with
    | UFst   -> "fst"
    | USnd   -> "snd"
    | UTail  -> "tl"
    | UHead  -> "hd"
    | UEmpty -> "empty"
    | UBang  -> "!"
    | URef   -> "ref"
    | UNth n -> "nth "^(string_of_int n)
	
let string_of_bin_op (op:binOpExpression) : string=
  match op with
    | BAdd  -> "+"
    | BSub  -> "-"
    | BMult -> "*"
    | BDiv  -> "/"      
    | BLEq  -> "<="       
    | BGEq  -> ">="
    | BGT   -> ">"
    | BLT   -> "<"
    | BEq   -> "="
    | BSetEq -> "!="
    | BSeq   -> ";"

let string_of_bool_op (op:boolOp): string =
  match op with
    |BAnd  -> "and"
    |BOr   -> "or"

let val_to_exp v =
  match v with     
    | VNaN     -> ENaN
    | VUnit    -> EUnit
    | VEmptyList t -> EEmptyList t
    | VPtr  n  -> EPtr n
    | VInt   n -> EInt n
    | VBool  b -> EBool b
    | VFloat f -> EFloat f
    | VCons (first,rest) -> ECons(first,rest)
    | VTuple (n,elst) -> ETuple (n,elst)
    | VFun (t1,t2,v,e) -> EFun (t1,t2,v,e)
    | VFix(t1,t2,v1,v2,e) -> EFix(t1,t2,v1,v2,e)
      
let rec string_of_expression (e:exp): string =
  match e with
    | ENaN     -> "NaN"
    | EPtr  n  -> "Ptr: "^string_of_int  n
    | EEmptyList t -> " [] : "^(string_of_type t)
    | ECons (first,rest) -> "( Cons "^(string_of_expression first)^
      " "^(string_of_expression rest)^" )"
    | EUnit    -> "()"
    | EVar (Var vname) -> vname
    | EInt  n  -> string_of_int  n
    | EBool b  -> string_of_bool b
    | EFloat f -> string_of_float f
    | ETuple (n,exp_list) -> "( tuple "^
      (let string_of_expression_space e = (string_of_expression e) ^" "
      in
       List.fold_left (^) "" (List.map string_of_expression_space exp_list))
      ^ ")"
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
    | EWhile (e1,e2)   -> "( while "^(string_of_expression e1)
      ^" "^(string_of_expression e2) ^" )"
    | EApp (e1,e2)           -> "( "^ (string_of_expression e1)
      ^" "^(string_of_expression e2) ^" )"
      
and string_of_value (v:value) : string=
  let exp_val_to_string e = (string_of_value (exp_to_value e))
  in
  match v with
    | VNaN     -> "NaN"
    | VUnit    -> "()"
    | VPtr   n -> "Ptr: "^string_of_int n
    | VInt   n -> string_of_int n
    | VBool  b -> string_of_bool b
    | VFloat f -> string_of_float f
    | VEmptyList t -> "[] : "^(string_of_type t)
    | VCons (first, rest) -> "["^(exp_val_to_string first)^(rest |> string_of_list)
    | VTuple (n,e1::exp_list) -> "( "^(exp_val_to_string e1)
      ^(string_of_tuple (n-1) exp_list)^" )"
    | VTuple (n,[])     -> ""
    | VFun (_,_,Var vname,e) -> "fun "^ vname^" ->" ^ string_of_expression e
    | VFix (_,_,Var vname1,Var vname2,e) ->
      "fix "^ vname1^" "^vname2^" ->" ^ string_of_expression e

and string_of_tuple (n:int) (exp_list:exp list) :string =
  let exp_val_to_string e = (string_of_value (exp_to_value e))
  in
  match (n,exp_list) with
    | (0,_)   -> ""
    | (1,hd::tl)   -> " , "^exp_val_to_string hd
    | (_,[])   -> failwith
      (Printf.sprintf "Malformed type: Tuple")
    | (n,hd::tl)   -> " , "
      ^(exp_val_to_string hd)^(string_of_tuple (n-1) tl)
      
	
and string_of_list (e:exp) :string=
  match e with
      | EEmptyList t   -> "] :"^(string_of_type t)
      | ECons (first, rest) -> " ,"^(string_of_expression first)^(string_of_list rest)
      | _                   -> (string_of_expression e)^"]"
	
and string_of_type (t:typ) : string=
  match t with
    | TNaN         -> "NaN"
    | TUnit        -> "()"
    | TInt         -> "int"
    | TFloat       -> "float"
    | TBool        -> "bool"
    | TRef  t      -> "<"^(string_of_type t)^">"
    | TList  t      -> "["^(string_of_type t)^"]"
    | TTuple (n,t_list) ->"( "^ (string_of_tuple_type n t_list)^" )"
    | TArrow (t1,t2) ->
      "( "^(string_of_type t1) ^ "->" ^ (string_of_type t2)^" )"

and string_of_tuple_type (n:int) (t_list:typ list) :string=
  match (n,t_list) with
    | (0,_) -> ""
    | (1,hd::[]) -> string_of_type hd
    | (n, hd::tl) -> (string_of_type hd)^" * "^(string_of_tuple_type (n-1) tl)
    | _         ->failwith
      (Printf.sprintf "Malformed type: Tuple")

and  exp_to_value e =
  match e with     
    | ENaN     -> VNaN
    | EUnit    -> VUnit
    | EPtr n    -> VPtr n
    | EEmptyList t   -> VEmptyList t
    | EInt   n -> VInt n
    | EBool  b -> VBool b
    | EFloat f -> VFloat f
    | ECons (first,rest) -> VCons(first,rest)
    | ETuple (n,e_list) -> VTuple (n,e_list)
    | EFun (t1,t2,v,e) -> VFun (t1,t2,v,e)
    | EFix(t1,t2,v1,v2,e) -> VFix(t1,t2,v1,v2,e)
    | _             ->failwith
      (Printf.sprintf "Expected value instead found : %s"
	 (string_of_expression e))

let rec is_value (e:exp) =
  match e with
    | ENaN |EUnit |EInt _ | EPtr _| EBool _
    | EFloat _ |EEmptyList _| EFun _| EFix _ -> true
    |  ETuple (_,[]) -> false
    |  ETuple (_,hd::[]) -> is_value hd
    |  ETuple (_,lst) -> List.fold_left (&&) true  (List.map is_value lst)
    |  ECons (e1,e2) -> is_value e1 && is_value e2
    | _ -> false      

      
let rec subst (v :value) (var:variable) (e:exp) : exp =
  let sub = subst v var in
  let (Var varname) = var in
  match e with
    | ENaN             -> ENaN
    | EUnit            -> EUnit
    | EPtr   n         -> EPtr n   
    | EEmptyList t     -> EEmptyList t
    | EInt   n         -> EInt n      
    | EBool  b         -> EBool b
    | EFloat f         -> EFloat f
    | ECons (e1,e2)    -> ECons(sub e1, sub e2)
    | ETuple (n,e_list) -> ETuple(n, List.map sub e_list)
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
    | EWhile (e1,e2)    -> EWhile(sub e1,sub e2)
	
    
let rec eval (env:environment) (e:exp) : environment*value =
    if (is_value e)
    then (env,exp_to_value e)
    else let (env,e) = step env e in eval env e

and eval_bool_op (env:environment) (op:boolOp) (e1:exp) (e2:exp) : (environment*value) =
  let (env, v1) = eval env e1 in
  let (env,v2) = eval env e2 in
  match (op,v1,v2) with
    | (BAnd,VBool v1, VBool v2)  -> (env, VBool (v1 && v2))
    | (BOr,VBool v1, VBool v2)   -> (env, VBool (v1 || v2))
    | _ -> failwith
      (Printf.sprintf "Was expecting a Boolean, instead found :(%s %s %s)"
	 (string_of_bool_op op)
	 (string_of_value v1)
	 (string_of_value v2))

and eval_uni_op (env:environment) (op:uniOpExpression) (e:exp):
    (environment*value) =
  let (env,v) = eval env e in
  let rec get_nth n e_list =
    match (n,e_list) with
    | (0,hd::_) ->hd
    | (n, hd::tl) -> get_nth (n-1) tl
    | (n, [])   ->failwith
      (Printf.sprintf "Malformed type: tuple")
  in
  match (op,v) with
    | (UFst, VTuple (_,hd::_)) -> eval env hd
    | (USnd, VTuple (_,hd1::hd2::_)) -> eval env hd2
    | (UNth n1, VTuple (n2,e_list)) ->if n1< n2 then
	eval env (get_nth n1 e_list)
      else
	failwith
      (Printf.sprintf "Index out of bounds exception in :(%s %s)"
	 (string_of_uni_op op)
	 (string_of_value v))
    | (UFst,_) | (USnd,_)|(UNth _,_) -> failwith
      (Printf.sprintf "Was expecting Tuple instead found :(%s %s)"
	 (string_of_uni_op op)
	 (string_of_value v))   
    | (UHead, VCons (v1,v2)) -> eval env v1
    | (UTail, VCons (v1,v2)) -> eval env v2
    | (UEmpty, VCons (v1,v2)) -> (env ,VBool false)
    | (UEmpty, VEmptyList _) -> (env,VBool true)
    | (UHead ,_)|(UTail ,_)->
       failwith
      (Printf.sprintf "Was expecting Non-empty List, instead found :(%s %s)"
	 (string_of_uni_op op)
	 (string_of_value v))
    | (UEmpty ,_)->
       failwith
      (Printf.sprintf "Was expecting List, instead found :(%s %s)"
	 (string_of_uni_op op)
	 (string_of_value v))
    | (UBang,VPtr n ) -> begin
      try (env,List.assoc n env)
      with _ -> failwith
	(Printf.sprintf "Null pointer found in :(%s %s)"
	   (string_of_uni_op op)
	   (string_of_value v))
	end
    | (UBang,v) -> failwith
	(Printf.sprintf "Attempted to derefrence non pointer: %s"
	   (string_of_value v))
    | (URef,v) -> let n = (List.length env) in ((n,v)::env, VPtr n)
	  
      
and eval_bin_op (env:environment) (op:binOpExpression) (e1:exp) (e2:exp)=
  let (env,v1) = eval env e1 in
  let (env,v2) = eval env e2 in
  (* taken from partially from
   * https://stackoverflow.com/questions/37091784/ocaml-function-replace-a-element-in-a-list
   *)
   let replace lst position replacement = 
     List.map
       (fun (index,x) -> if index = position then (index,replacement) else (index,x))
       lst
   in
  match (op,v1,v2) with
    | (BSeq,v1,v2)   -> (env,v2)
    | (BSetEq, VPtr n, v) -> (replace env n v, VUnit)
    | (BSetEq, _, _) -> failwith
      (Printf.sprintf "Attempted to assign value to non pointer in :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
    | (_,VBool _,_) |  ( _,_,VBool _)  -> failwith
      (Printf.sprintf "Was expecting number, instead found :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
    | (_,VNaN,_)           -> (env, VNaN)
    | (_,_,VNaN)	         -> (env,VNaN)
    | (_,VInt n1, VInt n2) -> eval_bin_op_int env op n1 n2
    | (_,VInt n1, VFloat f2) -> eval_bin_op_float env op (float_of_int n1) f2
    | (_,VFloat f1, VInt n2) -> eval_bin_op_float env op f1 (float_of_int n2)
    | (_,VFloat f1, VFloat f2) -> eval_bin_op_float env op f1 f2
    | _ -> failwith
      (Printf.sprintf "Was expecting number, instead found :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_value v1)
	 (string_of_value v2))
      
and eval_bin_op_int  (env:environment) (op:binOpExpression) (n1:int) (n2:int)=
  (env, match op with
    | BAdd  -> VInt( n1 +  n2)
    | BSub  -> VInt( n1 - n2)
    | BMult -> VInt( n1 * n2)
    | BLEq  -> VBool (n1 <= n2)
    | BGEq  -> VBool (n1 >= n2)
    | BLT  ->  VBool (n1 < n2)
    | BGT  ->  VBool (n1 > n2)
    | BEq ->   VBool (n1 = n2)             
    | BDiv  -> if n2 ==0 then VNaN else VInt( n1 / n2)
    | BSeq  -> VInt n2
    | BSetEq -> failwith
      (Printf.sprintf "Attempted to assign value to non pointer in :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_int n1)
	 (string_of_int n2)))

and eval_bin_op_float (env:environment) (op:binOpExpression) (f1:float) (f2:float)=
  (env,match op with
    | BAdd   -> VFloat( f1 +.  f2)
    | BSub   -> VFloat( f1 -. f2)
    | BMult  -> VFloat( f1 *. f2)
    | BLEq   -> VBool (f1 <= f2)
    | BGEq   -> VBool (f1 >= f2)
    | BLT    ->  VBool (f1 < f2)
    | BGT    ->  VBool (f1 > f2)
    | BEq    ->   VBool (f1 = f2)   
    | BDiv   -> if f2 ==0.0 then VNaN else VFloat( f1 /. f2)
    | BSeq   -> VFloat f2
    | BSetEq -> failwith
      (Printf.sprintf "Attempted to assign value to non pointer in :(%s %s %s)"
	 (string_of_bin_op op)
	 (string_of_float f1)
	 (string_of_float f2)))

and step (env:environment) (e:exp) : (environment*exp) =
  match e with
    | ENaN             -> (env,ENaN)
    | EPtr n           -> (env, EPtr n)
    | EEmptyList t     -> (env,EEmptyList t)
    | EUnit            -> (env,EUnit)
    | EInt   n         -> (env,EInt n)
    | EBool  b         -> (env,EBool b)
    | EFloat f         -> (env,EFloat f)
    | ECons (e1,e2)    -> step_cons env e1 e2
    | ETuple (n,e_list)    -> step_tuple env n e_list
    | EFun (t1,t2,var,e)        -> (env,EFun (t1,t2,var,e))
    | EFix (t1,t2,var1,var2,e)  -> (env,EFix (t1,t2,var1,var2,e))
    | EWhile (e1,e2)            -> (env,EIF (e1, EBin(BSeq,e2,e),EUnit))
    | EUni (op,e)      -> step_uni_op env op e
    | EBin (op,e1, e2) -> step_bin_op env op e1 e2      
    | EBinBool (op,e1, e2) -> step_bool_op env op e1 e2
    | EIF (e1,e2,e3)   -> step_if env e1 e2 e3
    | ELet (t,var, e1,e2) -> if is_value e1
      then (env,subst (exp_to_value e1) var e2)
      else  let (env,e1) = step env e1 in (env, ELet (t,var, e1,e2))
    | EApp (e1 , e2)    -> let (env,e) =step_app env e1 e2 in  (env,e)
    | EVar (Var var1)     -> failwith
      (Printf.sprintf "Unbound variable :%s"
	 var1)

and step_cons (env:environment) (e1:exp) (e2:exp) : (environment*exp)=
  if is_value e1 then
    begin
      if is_value e2 then (env,ECons (e1,e2))
      else let (env, e2) =(step env e2) in (env,ECons(e1,e2))
    end
  else let (env, e1) =(step env e1) in (env,ECons(e1,e2))

and step_tuple (env:environment) (n:int) (e_list:exp list) : environment*exp =
  let rec step_tuple_help env e_list =
    (match e_list with
      | (hd::[]) -> let (env,e) = step env hd in (env,e::[])
      | (hd::tl) -> if is_value hd
	then let (env,e_list) = step_tuple_help env tl in (env,hd::e_list)
	else let (env,e)      = step env hd in (env,e::tl)
      | _ -> failwith
      (Printf.sprintf "Malformed type: Tuple"))
  in
  if is_value (ETuple (n,e_list)) then (env,(ETuple (n,e_list)))
  else let (env ,e_list) =  step_tuple_help env e_list
       in  (env,(ETuple (n,e_list)))

and step_uni_op (env:environment) (op:uniOpExpression) (e:exp): (environment*exp) =
  if is_value e then
    let (env,v) = (eval_uni_op env op e) in (env,val_to_exp v)
  else let (env,e) = step env e in (env,EUni (op, e))
    
and step_bin_op (env:environment) (op:binOpExpression) (e1:exp) (e2:exp): (environment*exp) =
  if is_value e1 then
    begin
      if is_value e2 then let (env,v) =(eval_bin_op env op e1 e2) in (env, val_to_exp v)
      else let (env,e2) = step env e2 in (env,EBin (op, e1, e2))
    end
  else  let (env,e1) = step env e1 in (env,EBin (op, e1, e2))
      
and step_bool_op (env:environment) (op:boolOp) (e1:exp) (e2:exp): (environment*exp) =
  if is_value e1 then
    begin
      if is_value e2 then let (env,v) = (eval_bool_op env op e1 e2) in (env, val_to_exp v)
      else let (env,e2) = step env e2 in (env,EBinBool (op, e1, e2))
    end
  else let (env,e1) = (step env e1) in (env,EBinBool (op, e1, e2))

and step_if (env:environment) (e1:exp) (e2:exp) (e3:exp) : (environment*exp) =
  if is_value e1 then
    let v = exp_to_value e1
    in
    begin
      match v with
	| VBool true -> (env,e2)
	| VBool false -> (env,e3)
	|  _ -> failwith
	  (Printf.sprintf
	     "Type error, was expecting bool instead got: %s from %s"
	     (string_of_value v) (string_of_expression e1))
    end
  else let (env,e1) = step env e1 in (env, EIF (e1,e2,e3))

and step_app (env:environment) (e1:exp) (e2:exp) : (environment*exp) =
  if is_value e1 then
    if is_value e2 then
      begin
      match e1 with
	| EFun (_,_,var,e3) -> (env, subst (exp_to_value e2) var e3)
	| EFix (_,_,var1,var2,e3) as vF ->
	  (env,subst (exp_to_value vF) var1 (subst (exp_to_value e2) var2 e3))
	| _ -> failwith
	  (Printf.sprintf "Was expecting a function, instead found :%s %s"
	     (string_of_expression e1)
	     (string_of_expression e2))
      end
    else  let (env,e2) = step env e2 in (env, EApp ( e1, e2))
  else
     let (env,e1) = step env e1  in (env, EApp (  e1, e2))

  
let rec typecheck (c:ctx) (e:exp) : typ =
  let check_context v = List.assoc v c 
  in
  match e with
    | ENaN   -> TNaN
    | EUnit   -> TUnit
    | EPtr n  -> TInt
    | EEmptyList t  -> TList t
    | EVar v ->  begin
      try check_context v
      with _ ->
	failwith
	(Printf.sprintf "Unbound variable %s"
	   (string_of_expression e))
    end
    | EInt _ -> TInt
    | EFloat _ -> TFloat
    | EBool _  -> TBool
    | ECons (e1,e2) -> typecheck_list c e1 e2
    | ETuple (n,e_list) -> TTuple (n,(List.map (typecheck c) e_list))
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
    | EWhile (e1,e2) -> TUnit
    | EApp (e1,e2) -> typecheck_app c e1 e2

and typecheck_list (c:ctx) (first:exp) (rest:exp) : typ =
  let t1 = typecheck c first in
  let t2 = typecheck c rest in
  match (t1, t2) with
    | (first_t, TList rest_t) -> if first_t = rest_t
      then TList rest_t
      else failwith
	(Printf.sprintf "Type Error, expected %s in\n %s"
	   (string_of_type rest_t)
	   (string_of_expression (ECons(first,rest))))
    | _  -> failwith
      (Printf.sprintf "Type Error, appending to non list in\n %s"
	 (string_of_expression (ECons (first,rest))))
      
and typecheck_uni_op (c:ctx) (op:uniOpExpression) (e:exp) : typ =
  let t = typecheck c e in
  match (op,t) with
    | (UFst, TTuple (n,t1::tl)) -> t1
    | (USnd, TTuple (n,t1::t2::_)) -> t2
    | (UNth n1, TTuple (n2, t_list)) -> if n1<n2 then
	List.nth t_list n1
      else failwith
       (Printf.sprintf "%s applied to %s in\n %s"
	  (string_of_uni_op op)
	  (string_of_type t)	  	  
	  (string_of_expression e))
 
    | (UFst, _)| (USnd, _)| (UNth _,_)-> failwith
		  (Printf.sprintf
		     "%s must be applied to a tuple, instead found : %s in\n %s"
		     (string_of_uni_op op)
		     (string_of_type t)
		     (string_of_expression e))
    | (UHead, TList t1) -> t1
    | (UTail, TList t1) -> TList t1
    | (UEmpty, TList t1) -> TBool
    | (UHead, _)| (UTail, _)| (UEmpty, _) -> failwith
       (Printf.sprintf "%s must be applied to a List, instead found : %s in\n %s"
	  (string_of_uni_op op)
	  (string_of_type t)
	  (string_of_expression e))
    | (UBang,TRef t)   ->  t
    | (UBang, t   )    -> failwith
	(Printf.sprintf "Attempted to derefrence non pointer of type: %s in\n %s"
	   (string_of_type t)
	   (string_of_expression e))
    | (URef,t)        -> TRef t
      
and typecheck_bin_op (c:ctx) (op:binOpExpression) (e1:exp) (e2:exp) : typ =
  let t1 = typecheck c e1 in
  let t2 = typecheck c e2 in
  let ret_typ = begin
    match op with
      | BAdd | BSub | BMult| BDiv -> t1
      | BSeq                      -> t2
      | BLEq | BGEq | BGT| BLT| BEq -> TBool
      | BSetEq                    -> TUnit
  end
  in
  match (op,t1, t2) with
    | (BSeq ,_,_)        -> ret_typ
    | (BSetEq,TRef t1,t2)    -> if t1=t2 then ret_typ else
	failwith
	(Printf.sprintf "Attempted to assign expression of type: %s to pointer of type %s in\n%s"
	   (string_of_type t2)
	   (string_of_type t1)
	   (string_of_expression (EBin(op,e1,e2))))
    | (BSetEq,t1,t2)    -> failwith
	(Printf.sprintf "Attempted to assign to non pointer of type: %s in\n %s"
	   (string_of_type t1)
	   (string_of_expression (EBin(op,e1,e2))))
    | (_,TInt, TInt)     -> ret_typ
    | (_,TFloat, TFloat) -> ret_typ
    | (_,TNaN,_) | (_,_,TNaN) -> if ret_typ =TBool then TBool else TNaN
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
