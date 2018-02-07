open Core

(* Inspiration, and some functions were taken from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lexer.ml
 *)

  
(*Token definitions and helper functions*)
type token = LParen | RParen | Num of int | Plus 

let string_of_token token = match token with
    LParen -> "("
  | RParen -> ")"
  | Num n  -> string_of_int n
  | Plus   -> "+"
  
let print_token (token:token) =
  Printf.printf "%s\n" (string_of_token token)

(*Expression definitions and helper functions*)
type binaryOp = Add

type exp = ENum of int | EBinOp of binaryOp*exp*exp



    
(*Lexer helper functions*)
let is_digit (ch:char) : bool =
  let code = Char.code ch in
  48 <= code && code <= 57

let digit_val ch =
  Char.code ch - Char.code '0'
    
let is_whitespace (ch:char) : bool =
  ch = ' ' || ch = '\012' || ch = '\n' || ch = '\r' || ch = '\t'

(* Pops the head of the stream and returns it, advancing the stream forward *)
let advance : char Stream.t -> char = Stream.next
  
(* Peeks at the head of the stream without advancing it forward*)
let peek (src:char Stream.t) : char =
  match Stream.peek src with
  | Some ch -> ch
  | None    -> failwith "Unexpected end of file encountered"

let is_empty (src:char Stream.t) : bool =
  try
    Stream.empty src; true
  with
    Stream.Failure -> false
      
(* Lexer *)
(* Note: lex contains two nested helper functions, lex_num and go *)
(* Taken from
 * https://github.com/psosera/csc312-example-compiler/blob/master/ocaml/src/lexer.ml
 *)
let lex (src:char Stream.t) : token list =
  let rec lex_num acc =
    if is_digit (peek src) then
      lex_num (acc ^ (Char.escaped (advance src)))
    else
      int_of_string acc
  in
  let rec go () =
    if not (is_empty src) then
      let ch = peek src in
      (* Note: the |> operator takes the result of the left-hand side
       * and feeds it as an argument to the function on the right-hand
       * side.  ignore has type 'a -> unit---it allows us to throw
       * away the return type of a function we don't care about *)
      match ch with
      | '(' -> advance src |> ignore; LParen :: go ()
      | ')' -> advance src |> ignore; RParen :: go ()
      | '+' -> advance src |> ignore; Plus :: go ()
      | _   ->
        if is_whitespace ch then
          begin advance src |> ignore; go () end
        else if is_digit ch then
          let n = lex_num "" in
          Num n :: go ()
        else
          failwith (Printf.sprintf "Unexpected character found: %c" ch)
    else
      []
  in
    go ()
    
(* Parser *)    


(*Command line and Main*)
let command =
  Command.basic
    ~summary:"Echo the command-line arguments back to the user, one argument per line."
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
		  +> anon ("filename" %: file)
    )
    (fun filename () ->
      let stream = (Stream.of_channel (open_in filename)) in
      List.iter print_token (lex stream)
    )

  
let () = Command.run ~version:"1.0" ~build_info:"RWO" command
