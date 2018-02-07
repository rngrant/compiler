(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lang.ml *)

type binOpExpression = BAdd | BSub | BMult| BDiv
    
type exp =
  | EInt of int
  | EBin of binOpExpression*exp * exp



let string_of_bin_op (op:binOpExpression) : string=
  match op with
    |BAdd  -> "+"
    |BSub  -> "-"
    |BMult -> "*"
    |BDiv  -> "/"

    
let rec string_of_expression (e:exp): string =
  match e with
    | EInt n          -> string_of_int n
    | EBin (op,e1,e2) -> "( "
      ^ (string_of_bin_op op) ^ " "
      ^ (string_of_expression e1)^ " "
      ^ (string_of_expression e2) ^" )"     
    
let rec interpret (e:exp) : int =
  let interpret_bin_op (op:binOpExpression) (e1:exp) (e2:exp)=
  match op with
    | BAdd  -> interpret e1 + interpret e2
    | BSub  -> interpret e1 - interpret e2
    | BMult -> interpret e1 * interpret e2
    | BDiv  -> let div = interpret e2 in
	       if div ==0 then
		 failwith
		   (Printf.sprintf "Division by zero:(/ %s %s)"
		      (string_of_expression e1)
		      (string_of_expression e2))
	       else interpret e1 / interpret e2
  in
  match e with
  | EInt n        -> n
  | EBin (op,e1, e2) ->interpret_bin_op op e1 e2
