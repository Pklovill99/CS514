type term =
  | Var of string
  | Abs of string * term
  | App of term * term

let rec size (t : term) : int =
  match t with
  | Var _ -> 1
  | Abs (_, t1) -> 1 + size t1
  | App (t1, t2) -> 1 + size t1 + size t2

let is_value (t : term) : bool =
  match t with
  | Abs (_, _) -> true
  | _ -> false