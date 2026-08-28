let input_from_command_line () =
  match Array.to_list Sys.argv with
  | _program_name :: [] ->
      Printf.printf "Enter an addition expression: %!";
      read_line ()
  | _program_name :: arguments -> String.concat " " arguments
  | [] -> failwith "The argument array contained no program name"
 
let () =
  let input = input_from_command_line () in
  let lexbuf = Lexing.from_string input in
 
  try
    let expression = Parser.parse lexbuf in
 
    Printf.printf "\nInput:\n%s\n\n" input;
    Printf.printf "AST:\n%s\n\n" (Ast.to_tree expression);
    Printf.printf
      "Fully parenthesized:\n%s\n\n"
      (Ast.to_source expression);
    Printf.printf "Value:\n%d\n" (Ast.eval expression)
  with
  | Lexer.Lexing_error message ->
      Printf.eprintf "Lexing error: %s\n" message;
      exit 1
  | Parser.Parse_error message ->
      Printf.eprintf "Parsing error: %s\n" message;
      exit 1
 