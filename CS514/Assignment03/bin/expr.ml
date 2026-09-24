open Lambda_expression

let evaluate text =
  match Parser.parse text with
  | Error error ->
      Printf.eprintf "Parse error %s\n" (Parser.string_of_error error)
  | Ok expression ->
      Printf.printf "AST: %s\n" (Pretty.to_string expression);
      let free = Syntax.free_vars expression in
      if free <> [] then
        Printf.printf "Free variables: %s\n" (String.concat ", " free);
      (match Eval.eval expression with
       | Ok value -> Printf.printf "Value: %s\n" (Eval.string_of_value value)
       | Error error -> Printf.eprintf "Evaluation error: %s\n" (Eval.string_of_error error))

let repl () =
  print_endline "Expression REPL. Enter :quit to exit.";
  let rec loop () =
    print_string "> ";
    flush stdout;
    match read_line () with
    | exception End_of_file -> ()
    | ":quit" | ":q" -> ()
    | line ->
        evaluate line;
        loop ()
  in
  loop ()

let () =
  match Array.to_list Sys.argv with
  | [_] -> repl ()
  | [_; expression] -> evaluate expression
  | _ ->
      Printf.eprintf "usage: dune exec ./bin/expr.exe -- 'EXPRESSION'\n";
      exit 2
