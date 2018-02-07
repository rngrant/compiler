(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lexer.ml *)

type binOpToken = BTPlus| BTMinus| BTTimes | BTDivide | BTLessEq

type token =
  | TInt of int
  | TBool of bool
  | TLParen
  | TRParen
  | TBinOp of binOpToken
  | TIF

let string_of_token (t:token) : string =
  let string_of_op op =
    match op with
      | BTPlus   -> "+"
      | BTMinus  -> "-"
      | BTTimes  -> "*"
      | BTDivide -> "/"
      | BTLessEq -> "<="
  in
  let string_of_bool b = if b then "true" else "false"
  in 
  match t with
    | TBool b -> string_of_bool b
    | TInt n  -> string_of_int n
    | TLParen -> "("
    | TRParen -> ")"    
    | TBinOp op -> string_of_op op
    | TIF       -> "if"

let string_of_token_list (toks:token list) : string =
  String.concat "," (List.map string_of_token toks)

(* Peeks at the head of the stream without advancing it forward *)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next

(* Returns true iff this stream still has elements left *)
let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false

let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

let is_operator (ch:char) :bool =
  let operators = ['=';'+';'-';'<';'>'; '/'] in
  List.fold_left (fun a b -> a||b) false (List.map (fun c -> c== ch) operators)

let is_alpha (ch:char) :bool =
  let alpha = ['t';'r';'u';'e';'f'; 'a';'l';'s'] in
  List.fold_left (fun a b -> a||b) false (List.map (fun c -> c== ch) alpha)
    
(* Note: lex contains two nested helper functions, lex_num and go *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc =
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
       TInt (int_of_string acc)
  in
  let rec lex_words acc =
    if is_alpha (peek src) then
      lex_words (acc ^ (Char.escaped (advance src)))
    else
      match acc with
	| "true"  -> TBool true
	| "false" -> TBool false
	| "if"    -> TIF
	|    _    -> failwith (Printf.sprintf "Unbound value found: %s" acc)
  in
  let rec lex_operator acc =
    if is_operator (peek src) then
      lex_operator (acc ^ (Char.escaped (advance src)))
    else
      match acc with
	| "<=" -> TBinOp BTLessEq
	|  _   ->  failwith (Printf.sprintf "Unexpected operator found: %s" acc)
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src |> ignore; TLParen :: go ()
      | ')' -> advance src |> ignore; TRParen :: go ()
      | '+' -> advance src |> ignore; (TBinOp BTPlus) :: go ()
      | '-' -> advance src |> ignore; (TBinOp BTMinus) :: go ()
      | '*' -> advance src |> ignore; (TBinOp BTTimes) :: go ()
      | '/' -> advance src |> ignore; (TBinOp BTDivide) :: go ()	
      | _   ->
        if is_whitespace ch then
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let num_token = lex_num "" in
          num_token :: go ()
	else if is_operator ch then
	  let op_token = lex_operator "" in
	  op_token :: go ()
	else if is_alpha ch then
	  let word_token = lex_words "" in
	  word_token :: go ()
        else
          failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else
      []
  in
    go ()
