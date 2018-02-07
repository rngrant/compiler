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

let rec lex_num (stream:char Stream.t) accumulator :int =
  if is_digit (peek stream) then
    lex_num stream (accumulator ^ (Char.escaped (Stream.next stream)))
  else
    int_of_string accumulator
      
(* Lexer *)
let lex (stream:char Stream.t) : token list =
  let tokens = ref [] in
  (try
    while (not (is_empty stream)) do
      let ch = Stream.next stream in
      match ch with
	| '+' -> advance stream |> ignore; tokens := Plus::!tokens
	| '(' -> advance stream |> ignore; tokens := LParen::!tokens
	| ')' -> advance stream |> ignore; tokens := RParen::!tokens
	| _   ->
	  if (is_whitespace ch) then
	    advance stream |> ignore
	  else if (is_digit ch)  then begin advance stream |> ignore; tokens := Num (lex_num stream "")::!tokens end
   else
    failwith (Printf.sprintf "Unexpected character found: %c" ch)
    done
  with
      End_of_file -> ());
  List.rev !tokens


    
    
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
