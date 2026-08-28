type t =
  | Int of int
  | Plus
  | Minus
  | Star
  | Slash
  | LParen
  | RParen
  | Eof
 
let to_string = function
  | Int n -> Printf.sprintf "Int(%d)" n
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Eof -> "Eof"
 