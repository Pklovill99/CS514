open Syntax

let precedence = function
  | Let _ | LetRec _ | If _ | Fun _ -> 0
  | Binop ((And | Or), _, _) -> 1
  | Binop ((Eq | Lt | Le | Gt | Ge), _, _) -> 2
  | Binop ((Add | Sub), _, _) -> 3
  | Binop (Mul, _, _) -> 4
  | Unop _ -> 5
  | App _ -> 6
  | Var _ | Int _ | Bool _ -> 7

let to_string expr =
  let rec render context expression =
    let own = precedence expression in
    let text =
      match expression with
      | Var name -> name
      | Int value -> string_of_int value
      | Bool value -> string_of_bool value
      | Fun (parameter, body) ->
          Printf.sprintf "fun %s -> %s" parameter (render 0 body)
      | Let (name, definition, body) ->
          Printf.sprintf "let %s = %s in %s"
            name (render 0 definition) (render 0 body)
      | LetRec (name, definition, body) ->
          Printf.sprintf "let rec %s = %s in %s"
            name (render 0 definition) (render 0 body)
      | If (condition, then_branch, else_branch) ->
          Printf.sprintf "if %s then %s else %s"
            (render 0 condition) (render 0 then_branch) (render 0 else_branch)
      | Unop (operator, argument) ->
          string_of_unop operator ^ render 5 argument
      | App (operator, argument) ->
          Printf.sprintf "%s %s" (render 6 operator) (render 7 argument)
      | Binop (operator, left, right) ->
          let p = precedence expression in
          Printf.sprintf "%s %s %s"
            (render p left)
            (string_of_binop operator)
            (render (p + 1) right)
    in
    if own < context then "(" ^ text ^ ")" else text
  in
  render 0 expr
