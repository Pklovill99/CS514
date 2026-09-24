open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * environment
  | VBox of value ref
and environment = value Env.t

type error =
  | Unbound_variable of string
  | Expected_function
  | Expected_boolean
  | Expected_integers of binop
  | Expected_booleans of binop
  | Incomparable_values

let string_of_error = function
  | Unbound_variable name -> Printf.sprintf "unbound variable %S" name
  | Expected_function -> "attempted to apply a non-function value"
  | Expected_boolean -> "if condition did not evaluate to a boolean"
  | Expected_integers op ->
      Printf.sprintf "operator %s expects integer operands" (string_of_binop op)
  | Expected_booleans op ->
      Printf.sprintf "operator %s expects boolean operands" (string_of_binop op)
  | Incomparable_values -> "equality requires two integers or two booleans"

let rec force = function
  | VBox cell -> force !cell
  | value -> value

let string_of_value value =
  match force value with
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure _ -> "<fun>"
  | VBox _ -> "<fun>"

let string_of_environment env =
  env
  |> Env.to_list
  |> List.map (fun (name, value) -> name ^ " = " ^ string_of_value value)
  |> String.concat "; "
  |> fun text -> "[" ^ text ^ "]"

let lookup name env =
  match Env.lookup name env with
  | None -> Error (Unbound_variable name)
  | Some value -> Ok (force value)

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error error -> Error error

let empty_env = Env.empty

let prelude =
  Env.empty
  |> Env.extend "true" (VBool true)
  |> Env.extend "false" (VBool false)

let eval_binop operator left right =
  match operator, left, right with
  | Add, VInt a, VInt b -> Ok (VInt (a + b))
  | Sub, VInt a, VInt b -> Ok (VInt (a - b))
  | Mul, VInt a, VInt b -> Ok (VInt (a * b))
  | Eq, VInt a, VInt b -> Ok (VBool (a = b))
  | Eq, VBool a, VBool b -> Ok (VBool (a = b))
  | Lt, VInt a, VInt b -> Ok (VBool (a < b))
  | Le, VInt a, VInt b -> Ok (VBool (a <= b))
  | Gt, VInt a, VInt b -> Ok (VBool (a > b))
  | Ge, VInt a, VInt b -> Ok (VBool (a >= b))
  | And, VBool a, VBool b -> Ok (VBool (a && b))
  | Or, VBool a, VBool b -> Ok (VBool (a || b))
  | ((Add | Sub | Mul | Lt | Le | Gt | Ge) as op), _, _ ->
      Error (Expected_integers op)
  | ((And | Or) as op), _, _ -> Error (Expected_booleans op)
  | Eq, _, _ -> Error Incomparable_values

let eval ?(env = prelude) expr =
  let rec go environment expression =
    match expression with
    | Var name -> lookup name environment
    | Int value -> Ok (VInt value)
    | Bool value -> Ok (VBool value)
    | Unop (Neg, argument) ->
        let* value = go environment argument in
        (match force value with
         | VInt n -> Ok (VInt (-n))
         | _ -> Error (Expected_integers Sub))
    | Fun (parameter, body) ->
        Ok (VClosure (parameter, body, environment))
    | Let (name, definition, body) ->
        let* value = go environment definition in
        go (Env.extend name value environment) body
    | LetRec (name, definition, body) ->
        let cell = ref (VInt 0) in
        let environment' = Env.extend name (VBox cell) environment in
        let* value = go environment' definition in
        cell := value;
        go environment' body
    | If (condition, then_branch, else_branch) ->
        let* condition_value = go environment condition in
        (match force condition_value with
         | VBool true -> go environment then_branch
         | VBool false -> go environment else_branch
         | VInt _ | VClosure _ | VBox _ -> Error Expected_boolean)
    | App (operator, argument) ->
        let* operator_value = go environment operator in
        let* argument_value = go environment argument in
        (match force operator_value with
         | VClosure (parameter, body, closure_environment) ->
             go (Env.extend parameter argument_value closure_environment) body
         | VInt _ | VBool _ | VBox _ -> Error Expected_function)
    | Binop (And, left, right) ->
        let* left_value = go environment left in
        (match force left_value with
         | VBool false -> Ok (VBool false)
         | VBool true ->
             let* right_value = go environment right in
             (match right_value with
              | VBool b -> Ok (VBool b)
              | _ -> Error (Expected_booleans And))
         | _ -> Error (Expected_booleans And))
    | Binop (Or, left, right) ->
        let* left_value = go environment left in
        (match force left_value with
         | VBool true -> Ok (VBool true)
         | VBool false ->
             let* right_value = go environment right in
             (match right_value with
              | VBool b -> Ok (VBool b)
              | _ -> Error (Expected_booleans Or))
         | _ -> Error (Expected_booleans Or))
    | Binop (operator, left, right) ->
        let* left_value = go environment left in
        let* right_value = go environment right in
        eval_binop operator (force left_value) (force right_value)
  in
  go env expr
