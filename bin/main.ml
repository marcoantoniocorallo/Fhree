open Exceptions;;
open Utils;;
open Type_system;;

let cfa filename code = 
  let oc = open_out ((String.sub filename 0 ((String.length filename)-3))^".cfa") in 
  let annotated_prog = Cfa.to_aexpr code in
  Printf.fprintf oc "Annotated expression:\n\n%s\n\n" (Cfa.show_aexpr annotated_prog);
  let constraints = Cfa.constrs_of_aexpr annotated_prog in
  Printf.fprintf oc "Generated constraints:\n\n";
  List.iter (fun c ->
      Printf.fprintf oc "%s\n\n" (Cfa.Solver.show c)) constraints;
  let solution = Cfa.Solver.solve constraints in
  Printf.fprintf oc "Solution:\n\n";
  Seq.iter (fun (v, c) ->
      Printf.fprintf oc "%s =: [" (Cfa.Var.show v);
      Seq.iter (fun s -> Printf.fprintf oc "%s " (Cfa.Token.show s)) c;
      Printf.fprintf oc "]\n"
    ) solution

let eval code env = 
  let _ = type_check code in 
  print_endline (string_of_value (Interpreter.eval code env)) 

let print_usage () = 
  print_endline "Usage: Fhree [--no-cfa | --cfa | --all] filename.F3";
  print_endline ("Options:"^
  "\n\t--all"^
      "\n\t\t default option: does a control-flow-analysis of the code and prints the result into filename.cfa"^
      "\n\t\t then executes the program;"^
    "\n\t--no-cfa"^
      "\n\t\t executes only the program, without analyzing the code;"^
    "\n\t--cfa"^
      "\n\t\t executes only the control-flow analyzer, without executing the program;")
let format_error () = print_endline "Format Error: the file must have format .F3"

let () =
  let argvLength = Array.length Sys.argv in 
  if argvLength < 2 || argvLength > 3 then print_usage()
  else
    let opt = if argvLength = 3 then Sys.argv.(1) else "--all" in
    let filename = Sys.argv.(argvLength-1) in 
    if not (String.ends_with ~suffix:".F3" filename) then format_error()
    else let lexbuf = Lexing.from_channel (open_in filename) in 
      try
        let code = Parser.main Lexer.tokenize lexbuf in 
        match opt with
        | "--all" -> cfa filename code; eval code []
        | "--cfa" -> cfa filename code
        | "--no-cfa" -> eval code []
        | _ -> print_usage()
      with
        (* Character that doesn't matches any case in lexer (i.e. '&')*)
        |Lexing_Error(s) -> Printf.fprintf stderr "%s\n" s
        (* A malformed sequence of tokens (i.e. "let x + 5" ) *)
        |Parser.Error ->  Printf.fprintf stderr "Syntax error at %s.\n%!" 
                          (string_of_position (Lexing.lexeme_start_p lexbuf))
        (* Exceptions raised by the interpreter (i.e. a type error ) *)
        |exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn)