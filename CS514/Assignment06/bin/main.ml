open Syntax
open Context
open Typecheck
open Derivation
open Eval

let demo_term name t =
  Printf.printf "%s\n" name;
  Printf.printf "term: %s\n" (string_of_term t);
  match typeof empty_context t with
  | Error e ->
    Printf.printf "oops, type error: %s\n" (string_of_type_error e)
  | Ok ty ->
    Printf.printf "type is: %s\n" (string_of_ty ty);
    match derive empty_context t with
    | Ok (_, d) ->
      Printf.printf "proof tree:\n%s\n" (string_of_derivation_precise t d)
    | Error e ->
      Printf.printf "couldn't make derivation: %s\n" (string_of_type_error e);
    let result = eval t in
    Printf.printf "evaluates to: %s\n" (string_of_term result);
  Printf.printf "\n"

let () =
  let id_bool = Abs ("x", TyBool, Var "x") in
  let app_id = App (id_bool, True) in
  let higher =
    Abs ("f", TyArrow (TyBool, TyBool),
         Abs ("x", TyBool,
              App (Var "f", Var "x")))
  in
  demo_term "bool identity" id_bool;
  demo_term "applying the identity" app_id;
  demo_term "higher order thing" higher;
  (* some bad ones *)
  demo_term "true applied to false" (App (True, False));
  demo_term "identity on identity" (App (id_bool, id_bool));
  demo_term "if with different branch types"
    (If (True, True, Abs ("x", TyBool, Var "x")))