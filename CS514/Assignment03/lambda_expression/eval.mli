type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * Syntax.expr * environment
  | VBox of value ref
and environment = value Env.t

type error =
  | Unbound_variable of string
  | Expected_function
  | Expected_boolean
  | Expected_integers of Syntax.binop
  | Expected_booleans of Syntax.binop
  | Incomparable_values

val string_of_error : error -> string
val string_of_value : value -> string
val string_of_environment : environment -> string

val empty_env : environment
val prelude : environment

val eval : ?env:environment -> Syntax.expr -> (value, error) result
