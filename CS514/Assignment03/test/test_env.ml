let fail message =
  prerr_endline ("FAIL: " ^ message);
  exit 1

let check condition message =
  if not condition then fail message

let () =
  check (not (Env.is_bound "x" Env.empty)) "x is unbound in empty";
  check (Env.lookup "x" Env.empty = None) "lookup empty is None";

  let e1 = Env.extend "x" 10 Env.empty in
  check (Env.lookup "x" e1 = Some 10) "x -> 10";
  check (Env.is_bound "x" e1) "x is bound after extend";
  check (not (Env.is_bound "y" e1)) "y is still unbound";

  let e2 = Env.extend "y" 20 e1 in
  let e3 = Env.extend "z" 30 e2 in
  check (Env.lookup "x" e3 = Some 10) "x -> 10 in full env";
  check (Env.lookup "y" e3 = Some 20) "y -> 20 in full env";
  check (Env.lookup "z" e3 = Some 30) "z -> 30 in full env";

  check (not (Env.is_bound "y" e1)) "old environment is unchanged";

  let shadowed = Env.extend "x" 99 e3 in
  check (Env.lookup "x" shadowed = Some 99) "inner x shadows outer x";
  check (Env.lookup "x" e3 = Some 10) "outer x is intact";

  let prelude =
    Env.empty
    |> Env.extend "cout" "stdout"
    |> Env.extend "cin" "stdin"
    |> Env.extend "this" "object"
  in
  check (Env.is_bound "cin" prelude && Env.is_bound "cout" prelude) "pre-defined names";
  check (Env.lookup "this" prelude = Some "object") "this is pre-bound";

  print_endline "All environment tests passed."
