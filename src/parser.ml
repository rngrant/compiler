(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/parser.ml *)
open Lang
open Lexer

let rec peek : token list -> token = List.hd
let rec advance : token list -> token list = List.tl

let rec consume (t:token) (toks:token list) : token list =
  match toks with
  | t' :: toks ->
    if t = t' then
      toks
    else
      failwith (Printf.sprintf "Expected '%s', found '%s'" (string_of_token t) (string_of_token t'))
  | _ -> failwith "Encountered unexpected end of token stream"

let consume_bin_op  (toks:token list) : binOpExpression*token list =
  let op_exp op =
    match op with
      | BTPlus   -> BAdd
      | BTMinus  -> BSub
      | BTTimes  -> BMult
      | BTDivide -> BDiv
      | BTLessEq -> BLEq
  in
  match toks with
  | (TBinOp op) :: toks -> ((op_exp op),toks)
  | t:: toks ->  failwith (Printf.sprintf "Expected Operation, found '%s'" (string_of_token t))
  | _ -> failwith "Encountered unexpected end of token stream"
    
let rec parse (toks:token list) : (exp * token list) =
  if List.length toks = 0 then
    failwith "Unexpected end of token stream"
  else
    match peek toks with
      | TInt n  -> (EInt n, advance toks)
      | TBool b  -> (EBool b, advance toks)
      | TFloat f  -> (EFloat f, advance toks)	
      | TLParen -> begin
        let toks       = consume TLParen toks in
	match peek toks with
	  |TBinOp _ ->begin
	    let (op,toks)  = consume_bin_op toks in
            let (e1, toks) = parse toks in
            let (e2, toks) = parse toks in
            let toks       = consume TRParen toks in
            (EBin (op,e1, e2), toks)
	  end
	  | TIF -> begin
	    let toks  = consume TIF toks in
            let (e1, toks) = parse toks in
            let (e2, toks) = parse toks in
	    let (e3, toks) = parse toks in
            let toks       = consume TRParen toks in
            (EIF (e1, e2,e3), toks)
	  end
	  | t -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
      end
      | t      -> failwith (Printf.sprintf "Unexpected token found: %s" (string_of_token t))
