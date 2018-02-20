(*Starter code for this exercise copied from
 * https://github.com/psosera/csc312-example-compiler/ ...
 * blob/master/ocaml/src/compiler.ml *)
(* Used 
 *http://scylardor.fr/2013/10/14/ ...
 * ocaml-parsing-a-programs-arguments-with-the-arg-module/
 * for comand-line interface *)
open Lexer
open Parser
open Lang

let lex_mode = ref false
let parse_mode = ref false

let filename_to_tokens filename =
  Lexer.lex (Stream.of_channel (open_in filename))

let tokens_to_exp tokens =
  let (e, _) = Parser.parse tokens in e


let filename_to_value filename =
  filename_to_tokens filename
  |> tokens_to_exp
  |> Lang.interpret      

let input_handler filename lex parse =
  let filename_to_value_string filename =
    filename_to_tokens filename
  |> tokens_to_exp
  |> Lang.interpret
  |> Lang.string_of_value
  in
  let filename_to_exp_string filename =
    filename_to_tokens filename
    |> tokens_to_exp
    |> string_of_expression
  in
  let filename_to_token_string filename =    
    filename_to_tokens filename |> string_of_token_list
  in
  match (lex,parse) with
    | (true,_)      -> filename_to_token_string filename
    | (false,true)  -> filename_to_exp_string   filename     
    | (false,false) -> filename_to_value_string filename

let main () = begin
  let speclist  =
    [("-lex", Arg.Set lex_mode, "Enables lex mode");
     ("-parse", Arg.Set parse_mode, "Enables parse mode")
    ]
  in
  let usage_msg =
    "This is a basic interpreter for a scheme like language." ^
      "Options available:"
  in Arg.parse speclist
  (fun filename ->  input_handler filename !lex_mode !parse_mode
      |> print_string) usage_msg
end
  
  

let _ = if !Sys.interactive then () else main ()

    
