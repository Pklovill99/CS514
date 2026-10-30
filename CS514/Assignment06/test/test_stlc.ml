open Syntax
open Context
open Typecheck
open Derivation
open Eval

let assert_eq name expected actual =
  if expected = actual then
    Printf.printf "PASS: %s\n" name
  else
    failwith (Printf.sprintf "FAIL: %s\n  expected: %s\n  actual: %s"
                name expected actual)

let assert_ok_ty name expected t =
  match typeof empty_context t with
  | Ok ty -> assert_eq name (string_of_ty expected) (string_of_ty ty)
  | Error e -> failwith (Printf.sprintf "FAIL: %s (unexpected error: %s)" name (string_of_type_error e))

let assert_err name t =
  match typeof empty_context t with
  | Error _ -> Printf.printf "PASS: %s\n" name
  | Ok ty -> failwith (Printf.sprintf "FAIL: %s (expected error, got %s)" name (string_of_ty ty))

let () =
  let id = Abs ("x", TyBool, Var "x") in
  assert_ok_ty "1. Boolean identity" (TyArrow (TyBool, TyBool)) id;

  let app = App (id, True) in
  assert_ok_ty "2. application of Boolean identity" TyBool app;

  let higher =
    Abs ("f", TyArrow (TyBool, TyBool),
         Abs ("x", TyBool, App (Var "f", Var "x")))
  in
  assert_ok_ty "3. higher-order function"
    (TyArrow (TyArrow (TyBool, TyBool), TyArrow (TyBool, TyBool))) higher;

  assert_err "4. unbound variable" (Var "y");
  assert_err "5. application of a Boolean as a function" (App (True, False));
  assert_err "6. argument-type mismatch" (App (id, id));
  assert_err "7. branch-type mismatch"
    (If (True, True, Abs ("x", TyBool, Var "x")));

  let shadowed =
    Abs ("x", TyBool,
         Abs ("x", TyArrow (TyBool, TyBool),
              Var "x"))
  in
  assert_ok_ty "8. lexical shadowing"
    (TyArrow (TyBool, TyArrow (TyArrow (TyBool, TyBool), TyArrow (TyBool, TyBool))))
    shadowed;

  (match derive empty_context app with
   | Ok (ty, d) ->
     assert_eq "9a. derivation type" "Bool" (string_of_ty ty);
     let s = string_of_derivation_precise app d in
     if String.length s > 0 then Printf.printf "PASS: 9. successful derivation construction\n"
     else failwith "FAIL: 9. empty derivation string"
   | Error e -> failwith ("FAIL: 9. " ^ string_of_type_error e));

  (match derive empty_context (App (True, False)) with
   | Error (ExpectedFunction TyBool) ->
     Printf.printf "PASS: 10. useful reporting for a failed derivation\n"
   | Error e -> failwith ("FAIL: 10. wrong error: " ^ string_of_type_error e)
   | Ok _ -> failwith "FAIL: 10. expected error");

  assert_eq "eval of id true" "true" (string_of_term (eval app));

  Printf.printf "\nAll tests passed.\n"
