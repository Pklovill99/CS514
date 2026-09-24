type binop =
  | Add
  | Sub
  | Mul
  | Eq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or

type unop =
  | Neg

type expr =
  | Var of string
  | Int of int
  | Bool of bool
  | Fun of string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * expr * expr
  | If of expr * expr * expr
  | Binop of binop * expr * expr
  | Unop of unop * expr

val free_vars : expr -> string list
val string_of_binop : binop -> string
val string_of_unop : unop -> string
