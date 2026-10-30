type context = (string * Syntax.ty) list

let empty_context = []

let extend x ty ctx = (x, ty) :: ctx

let rec lookup x = function
  | [] -> None
  | (y, ty) :: rest -> if x = y then Some ty else lookup x rest