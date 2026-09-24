type 'a t = (string * 'a) list

let empty = []

let extend name value env = (name, value) :: env

let rec lookup name = function
  | [] -> None
  | (bound, value) :: rest ->
      if name = bound then Some value else lookup name rest

let is_bound name env =
  match lookup name env with
  | Some _ -> true
  | None -> false

let to_list env =
  let rec unique seen = function
    | [] -> []
    | (name, value) :: rest ->
        if List.mem name seen then unique seen rest
        else (name, value) :: unique (name :: seen) rest
  in
  unique [] env
