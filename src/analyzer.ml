let () =
  let channel =
    if Array.length Sys.argv > 1 then
      Sys.argv.(1) |> open_in
    else
      stdin
  in
  try
    let lexbuf = Lexing.from_channel channel in
    let prog = Parser.main Lexer.tokenize lexbuf in
    let annotated_prog = Cfa.to_aexpr prog in
    Printf.printf "Annotated expression:\n\n%s\n\n" (Cfa.show_aexpr annotated_prog);
    let constraints = Cfa.constrs_of_aexpr annotated_prog in
    Printf.printf "Generated constraints:\n\n";
    List.iter (fun c ->
        Printf.printf "%s\n\n" (Cfa.Solver.show c)) constraints;
    let solution = Cfa.Solver.solve constraints in
    Printf.printf "Solution:\n\n";
    Seq.iter (fun (v, c) ->
        Printf.printf "%s =: [" (Cfa.Var.show v);
        Seq.iter (fun s -> Printf.printf "%s " (Cfa.Token.show s)) c;
        Printf.printf "]\n"
      ) solution
  with
  | Exceptions.Lexing_Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "Syntax error.\n"