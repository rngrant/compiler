open Core

let command =
  Command.basic
    ~summary:"Echo the command-line arguments back to the user, one argument per line."
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty
		  +> flag "-length" no_arg ~doc:"prints the lengths of each of the arguments"
		  +> anon (sequence ("argument" %: string))
    )
    (fun use_length strings () ->
       match use_length with
	 | false ->  List.iter (fun string -> Printf.printf "%s\n"string) strings 
	 | true  ->  List.iter (fun string ->  Printf.printf "%d\n" (String.length string)) strings 
    )

  
let () = Command.run ~version:"1.0" ~build_info:"RWO" command
            
