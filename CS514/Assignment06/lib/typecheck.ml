open Syntax
open Context

type type_error =
  | UnboundVariable of string
  | ExpectedFunction of ty
  | ArgumentMismatch of ty * ty
  | ExpectedBool of ty
  | BranchMismatch of ty * ty

let string_of_type_error = function
  | UnboundVariable x -> "unbound var: " ^ x
  | ExpectedFunction ty -> "need a function, got " ^ string_of_ty ty
  | ArgumentMismatch (expected, actual) ->
    "arg type wrong: wanted " ^ string_of_ty expected ^ ", got " ^ string_of_ty actual
  | ExpectedBool ty -> "need Bool, got " ^ string_of_ty ty
  | BranchMismatch (then_ty, else_ty) ->
    "branches don't match: then is " ^ string_of_ty then_ty ^ ", else is " ^ string_of_ty else_ty

let rec typeof ctx t =
  match t with
  | Var x ->
    (match lookup x ctx with
     | Some ty -> Ok ty
     | None -> Error (UnboundVariable x))
  | True | False -> Ok TyBool
  | Abs (x, ty, body) ->
    (match typeof (extend x ty ctx) body with
     | Ok body_ty -> Ok (TyArrow (ty, body_ty))
     | Error e -> Error e)
  | App (f, a) ->
    (match typeof ctx f with
     | Error e -> Error e
     | Ok (TyArrow (arg_ty, res_ty)) ->
       (match typeof ctx a with
        | Error e -> Error e
        | Ok ty2 when ty2 = arg_ty -> Ok res_ty
        | Ok ty2 -> Error (ArgumentMismatch (arg_ty, ty2)))
     | Ok other -> Error (ExpectedFunction other))
  | If (c, t1, t2) ->
    (match typeof ctx c with
     | Error e -> Error e
     | Ok TyBool ->
       (match typeof ctx t1, typeof ctx t2 with
        | Ok ty1, Ok ty2 when ty1 = ty2 -> Ok ty1
        | Ok ty1, Ok ty2 -> Error (BranchMismatch (ty1, ty2))
        | Error e, _ | _, Error e -> Error e)
     | Ok other -> Error (ExpectedBool other))