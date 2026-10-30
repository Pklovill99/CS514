type ty =
  | TyBool
  | TyArrow of ty * ty

type term =
  | Var of string
  | Abs of string * ty * term
  | App of term * term
  | True
  | False
  | If of term * term * term

let rec string_of_ty = function
  | TyBool -> "Bool"
  | TyArrow (a, b) ->
    let left = match a with TyArrow _ -> "(" ^ string_of_ty a ^ ")" | _ -> string_of_ty a in
    left ^ " -> " ^ string_of_ty b

let rec string_of_term = function
  | Var x -> x
  | Abs (x, ty, body) -> "lambda " ^ x ^ ":" ^ string_of_ty ty ^ ". " ^ string_of_term body
  | App (f, a) -> string_of_term f ^ " " ^ string_of_term a
  | True -> "true"
  | False -> "false"
  | If (c, t, e) -> "if " ^ string_of_term c ^ " then " ^ string_of_term t ^ " else " ^ string_of_term e

let string_of_context ctx =
  if ctx = [] then "."
  else String.concat ", " (List.map (fun (x, t) -> x ^ ":" ^ string_of_ty t) (List.rev ctx))