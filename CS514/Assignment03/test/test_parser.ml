open Lambda_expression
open Syntax
open Eval

let fail message =
  prerr_endline ("FAIL: " ^ message);
  exit 1

let check condition message =
  if not condition then fail message

let parse source =
  match Parser.parse source with
  | Ok expression -> expression
  | Error error -> fail (source ^ ": " ^ Parser.string_of_error error)

let eval source =
  match Eval.eval (parse source) with
  | Ok value -> value
  | Error error -> fail (source ^ ": " ^ Eval.string_of_error error)

let eval_error source =
  match Eval.eval (parse source) with
  | Error error -> error
  | Ok value ->
      fail (source ^ ": expected an error, got " ^ string_of_value value)

let () =
  (* Lecture tests *)
  check
    (parse "x + y * z" = Binop (Add, Var "x", Binop (Mul, Var "y", Var "z")))
    "multiplication should bind more tightly than addition";

  check
    (parse "f x + 1" = Binop (Add, App (Var "f", Var "x"), Int 1))
    "application should bind more tightly than addition";

  check
    (parse "f x y" = App (App (Var "f", Var "x"), Var "y"))
    "application should associate to the left";

  check
    (parse "fun x y -> x + y" =
       Fun ("x", Fun ("y", Binop (Add, Var "x", Var "y"))))
    "multiple parameters should desugar to nested functions";

  check
    (Syntax.free_vars (parse "fun x -> x + y") = ["y"])
    "free variable analysis";

  check
    (eval "let x = 10 in x * 3 + 2" = VInt 32)
    "let binding and arithmetic";

  check
    (eval "(fun x -> x + 1) 41" = VInt 42)
    "function application";

  check
    (eval "let x = 10 in let f = fun y -> x + y in let x = 100 in f 5" = VInt 15)
    "closures must preserve lexical scope";

  check
    (eval "if 2 * 3 == 6 then 10 else 20" = VInt 10)
    "if and equality";

  check
    (parse "-3" = Unop (Neg, Int 3))
    "unary minus";

  check
    (parse "--5" = Unop (Neg, Unop (Neg, Int 5)))
    "nested unary minus";

  check
    (eval "-(2 + 3)" = VInt (-5))
    "unary minus of an additive expression";

  check
    (eval "3 < 4" = VBool true)
    "comparison <";

  check
    (eval "4 <= 4" = VBool true)
    "comparison <=";

  check
    (eval "3 > 4" = VBool false)
    "comparison >";

  check
    (eval "5 >= 1" = VBool true)
    "comparison >=";

  check
    (eval "true && false" = VBool false)
    "logical and";

  check
    (eval "true || false" = VBool true)
    "logical or";

  check
    (eval "false && (1 2)" = VBool false)
    "&& short-circuits; the right operand is not evaluated";

  check
    (match parse "let rec f = fun x -> f x in f" with
     | LetRec ("f", Fun ("x", App (Var "f", Var "x")), Var "f") -> true
     | _ -> false)
    "let rec parses as LetRec";

  check
    (eval
       "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1) in fact 5"
     = VInt 120)
    "let rec factorial";

  check (eval "(fun x -> x) 5" = VInt 5) "identity";
  check (eval "(fun x -> x * x) 9" = VInt 81) "square";
  check (eval "let add = fun x y -> x + y in add 2 3" = VInt 5) "curried add";
  check
    (eval
       "let apply_twice = fun f x -> f (f x) in apply_twice (fun x -> x + 1) 10"
     = VInt 12)
    "higher-order apply_twice";
  check
    (eval "let twice = fun f x -> f (f x) in twice (fun x -> x * 2) 3" = VInt 12)
    "higher-order twice";
  check (eval "(\\x. x + 1) 41" = VInt 42) "backslash lambda";

  (match eval_error "x" with
   | Unbound_variable "x" -> ()
   | _ -> fail "x should be an unbound variable");

  (match eval_error "3 4" with
   | Expected_function -> ()
   | _ -> fail "3 4 should be a non-function application");

  print_endline "All parser/evaluator tests passed."
