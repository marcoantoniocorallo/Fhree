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

let eval code = 
  let _ = type_check code in 
  print_endline (string_of_value (Interpreter.eval code)) 

let print_usage () = 
  print_endline "Usage: Fhree [--no-cfa | --cfa | --all] filename.F3";
  print_endline ("Options:"^
  "\n\t--no-cfa"^
      "\n\t\t default option: executes only the program, without analyzing the control-flow;"^
    "\n\t--cfa"^
      "\n\t\t executes only the control-flow analyzer, without executing the program;"^
    "\n\t--all"^
      "\n\t\t does a control-flow-analysis of the code and prints the result into filename.cfa"^
      "\n\t\t then executes the program;"
  )

let () =
  let argvLength = Array.length Sys.argv in 
  if argvLength < 2 || argvLength > 3 then print_usage()
  else
    let opt = if argvLength = 3 then Sys.argv.(1) else "--no-cfa" in
    let filename = Sys.argv.(argvLength-1) in 
    let lexbuf = ref None in  
    try
      lexbuf := Some (Lexing.from_channel (open_in filename));
      let code = Parser.main Lexer.tokenize (Option.get !lexbuf) in 
      match opt with
      | "--all" -> cfa filename code; eval code
      | "--cfa" -> cfa filename code
      | "--no-cfa" -> eval code
      | _ -> print_usage()
    with
      | Sys_error(s) -> Printf.fprintf stderr "%s\n" s
      (* Character that doesn't match any case in lexer (i.e. '&')*)
      | Lexing_Error(s) -> Printf.fprintf stderr "%s\n" s
      (* A malformed sequence of tokens (i.e. "let x + 5" ) *)
      | Parser.Error ->  Printf.fprintf stderr "Syntax error at %s.\n%!" 
                        (string_of_position (Lexing.lexeme_start_p (Option.get !lexbuf)))
      (* Type Error *)
      | Type_Error(s) -> Printf.fprintf stderr "%s\n" s
      (* Other exceptions raised by the interpreter *)
      | exn -> Printf.fprintf stderr "%s\n" (Printexc.to_string exn)
