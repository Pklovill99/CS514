type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Neg of expr
  | Pos of expr

let rec eval = function
  | Int n -> n
  | Add (left, right) -> eval left + eval right
  | Sub (left, right) -> eval left - eval right
  | Mul (left, right) -> eval left * eval right
  | Div (left, right) -> eval left / eval right
  | Neg e -> - (eval e)
  | Pos e -> eval e

let rec to_source = function
  | Int n -> string_of_int n
  | Add (left, right) ->
      Printf.sprintf "(%s + %s)" (to_source left) (to_source right)
  | Sub (left, right) ->
      Printf.sprintf "(%s - %s)" (to_source left) (to_source right)
  | Mul (left, right) ->
      Printf.sprintf "(%s * %s)" (to_source left) (to_source right)
  | Div (left, right) ->
      Printf.sprintf "(%s / %s)" (to_source left) (to_source right)
  | Neg e -> Printf.sprintf "(-%s)" (to_source e)
  | Pos e -> Printf.sprintf "(+%s)" (to_source e)

let rec to_tree ?(indent = 0) expression =
  let padding = String.make indent ' ' in

  match expression with
  | Int n -> Printf.sprintf "%sInt(%d)" padding n
  | Add (left, right) ->
      Printf.sprintf
        "%sAdd\n%s\n%s"
        padding
        (to_tree ~indent:(indent + 2) left)
        (to_tree ~indent:(indent + 2) right)
  | Sub (left, right) ->
      Printf.sprintf
        "%sSub\n%s\n%s"
        padding
        (to_tree ~indent:(indent + 2) left)
        (to_tree ~indent:(indent + 2) right)
  | Mul (left, right) ->
      Printf.sprintf
        "%sMul\n%s\n%s"
        padding
        (to_tree ~indent:(indent + 2) left)
        (to_tree ~indent:(indent + 2) right)
  | Div (left, right) ->
      Printf.sprintf
        "%sDiv\n%s\n%s"
        padding
        (to_tree ~indent:(indent + 2) left)
        (to_tree ~indent:(indent + 2) right)
  | Neg e ->
      Printf.sprintf "%sNeg\n%s" padding (to_tree ~indent:(indent + 2) e)
  | Pos e ->
      Printf.sprintf "%sPos\n%s" padding (to_tree ~indent:(indent + 2) e)