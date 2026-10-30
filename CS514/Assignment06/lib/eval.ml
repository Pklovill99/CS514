open Syntax

let is_value = function Abs _ | True | False -> true | _ -> false

let rec subst x v t =
  match t with
  | Var y -> if x = y then v else t
  | Abs (y, ty, body) -> if x = y then t else Abs (y, ty, subst x v body)
  | App (t1, t2) -> App (subst x v t1, subst x v t2)
  | True | False -> t
  | If (c, t1, t2) -> If (subst x v c, subst x v t1, subst x v t2)

let rec eval t =
  match t with
  | App (t1, t2) ->
    let t1' = eval t1 in
    let t2' = eval t2 in
    (match t1' with
     | Abs (x, _, body) when is_value t2' -> eval (subst x t2' body)
     | _ -> App (t1', t2'))
  | If (c, t1, t2) ->
    (match eval c with
     | True -> eval t1
     | False -> eval t2
     | c' -> If (c', t1, t2))
  | _ -> t