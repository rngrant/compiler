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

let parse_mode = ref false

let filename_to_tokens filename =
  Lexing.from_channel (open_in filename)

let tokens_to_exp tokens = Parser.main Lexer.token tokens

let filename_to_exp filename  =
  (open_in filename)
  |> Lexing.from_channel
  |> Parser.main Lexer.token

let input_handler filename parse =
  match parse with
    | true      -> filename_to_exp filename |> string_of_expression
    | false     -> filename_to_exp filename |> Lang.eval |> string_of_value

let main () = begin
  let speclist  =
    [("-parse", Arg.Set parse_mode, "Enables parse mode")
    ]
  in
  let usage_msg =
    "This is a basic interpreter for a scheme like language." ^
      "Options available:"
  in Arg.parse speclist
  (fun filename ->  input_handler filename !parse_mode
      |> print_string) usage_msg
end
  
  

let _ = if !Sys.interactive then () else main ()

    
