type error = {
  position : int;
  message : string;
}

val string_of_error : error -> string
val parse : string -> (Syntax.expr, error) result
val parse_exn : string -> Syntax.expr
