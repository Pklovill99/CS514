open Assignment02

let () =
  Printf.printf "size (Abs (\"x\", Var \"x\")) = %d\n" (size (Abs ("x", Var "x")));
  Printf.printf "is_value (Abs (\"x\", Var \"x\")) = %b\n" (is_value (Abs ("x", Var "x")));
  Printf.printf "is_value (Var \"x\") = %b\n" (is_value (Var "x"))