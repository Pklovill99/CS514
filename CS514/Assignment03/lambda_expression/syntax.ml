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

let string_of_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Eq -> "=="
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function
  | Neg -> "-"

module StringSet = Set.Make (String)

let free_vars expr =
  let rec walk bound expression =
    match expression with
    | Var name ->
        if StringSet.mem name bound then StringSet.empty
        else StringSet.singleton name
    | Int _ | Bool _ -> StringSet.empty
    | Fun (parameter, body) ->
        walk (StringSet.add parameter bound) body
    | App (operator, argument)
    | Binop (_, operator, argument) ->
        StringSet.union (walk bound operator) (walk bound argument)
    | Unop (_, argument) -> walk bound argument
    | Let (name, definition, body) ->
        StringSet.union
          (walk bound definition)
          (walk (StringSet.add name bound) body)
    | LetRec (name, definition, body) ->
        let bound' = StringSet.add name bound in
        StringSet.union (walk bound' definition) (walk bound' body)
    | If (condition, then_branch, else_branch) ->
        StringSet.union
          (walk bound condition)
          (StringSet.union
             (walk bound then_branch)
             (walk bound else_branch))
  in
  StringSet.elements (walk StringSet.empty expr)
