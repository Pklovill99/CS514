open Syntax

type error = {
  position : int;
  message : string;
}

let string_of_error error =
  Printf.sprintf "at character %d: %s" error.position error.message

type token_kind =
  | Lambda
  | Dot
  | Arrow
  | LParen
  | RParen
  | Plus
  | Minus
  | Star
  | Equal
  | EqualEqual
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | LetKw
  | RecKw
  | InKw
  | IfKw
  | ThenKw
  | ElseKw
  | TrueKw
  | FalseKw
  | Ident of string
  | Integer of int
  | End

type token = {
  kind : token_kind;
  position : int;
}

exception Internal_parse_error of error

let fail position message =
  raise (Internal_parse_error { position; message })

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let keyword_or_ident text =
  match text with
  | "fun" -> Lambda
  | "let" -> LetKw
  | "rec" -> RecKw
  | "in" -> InKw
  | "if" -> IfKw
  | "then" -> ThenKw
  | "else" -> ElseKw
  | "true" -> TrueKw
  | "false" -> FalseKw
  | _ -> Ident text

let tokenize input =
  let length = String.length input in
  let rec scan index reversed =
    if index >= length then
      List.rev ({ kind = End; position = length } :: reversed)
    else
      match input.[index] with
      | ' ' | '\t' | '\r' | '\n' -> scan (index + 1) reversed
      | '\\' -> scan (index + 1) ({ kind = Lambda; position = index } :: reversed)
      | '.' -> scan (index + 1) ({ kind = Dot; position = index } :: reversed)
      | '(' -> scan (index + 1) ({ kind = LParen; position = index } :: reversed)
      | ')' -> scan (index + 1) ({ kind = RParen; position = index } :: reversed)
      | '+' -> scan (index + 1) ({ kind = Plus; position = index } :: reversed)
      | '*' -> scan (index + 1) ({ kind = Star; position = index } :: reversed)
      | '-' when index + 1 < length && input.[index + 1] = '>' ->
          scan (index + 2) ({ kind = Arrow; position = index } :: reversed)
      | '-' -> scan (index + 1) ({ kind = Minus; position = index } :: reversed)
      | '=' when index + 1 < length && input.[index + 1] = '=' ->
          scan (index + 2) ({ kind = EqualEqual; position = index } :: reversed)
      | '=' -> scan (index + 1) ({ kind = Equal; position = index } :: reversed)
      | '<' when index + 1 < length && input.[index + 1] = '=' ->
          scan (index + 2) ({ kind = Le; position = index } :: reversed)
      | '<' -> scan (index + 1) ({ kind = Lt; position = index } :: reversed)
      | '>' when index + 1 < length && input.[index + 1] = '=' ->
          scan (index + 2) ({ kind = Ge; position = index } :: reversed)
      | '>' -> scan (index + 1) ({ kind = Gt; position = index } :: reversed)
      | '&' when index + 1 < length && input.[index + 1] = '&' ->
          scan (index + 2) ({ kind = And; position = index } :: reversed)
      | '|' when index + 1 < length && input.[index + 1] = '|' ->
          scan (index + 2) ({ kind = Or; position = index } :: reversed)
      | '0' .. '9' ->
          let stop = ref (index + 1) in
          while !stop < length &&
                (match input.[!stop] with '0' .. '9' -> true | _ -> false)
          do
            incr stop
          done;
          let text = String.sub input index (!stop - index) in
          let value =
            try int_of_string text
            with Failure _ -> fail index "integer literal is out of range"
          in
          scan !stop ({ kind = Integer value; position = index } :: reversed)
      | character when is_letter character ->
          let stop = ref (index + 1) in
          while !stop < length && is_identifier_char input.[!stop] do
            incr stop
          done;
          let text = String.sub input index (!stop - index) in
          scan !stop ({ kind = keyword_or_ident text; position = index } :: reversed)
      | character ->
          fail index (Printf.sprintf "unexpected character %C" character)
  in
  scan 0 []

type stream = {
  tokens : token array;
  mutable cursor : int;
}

let current stream = stream.tokens.(stream.cursor)

let advance stream =
  let token = current stream in
  stream.cursor <- stream.cursor + 1;
  token

let token_description = function
  | Lambda -> "'fun' or '\\'"
  | Dot -> "'.'"
  | Arrow -> "'->'"
  | LParen -> "'('"
  | RParen -> "')'"
  | Plus -> "'+'"
  | Minus -> "'-'"
  | Star -> "'*'"
  | Equal -> "'='"
  | EqualEqual -> "'=='"
  | Lt -> "'<'"
  | Le -> "'<='"
  | Gt -> "'>'"
  | Ge -> "'>='"
  | And -> "'&&'"
  | Or -> "'||'"
  | LetKw -> "'let'"
  | RecKw -> "'rec'"
  | InKw -> "'in'"
  | IfKw -> "'if'"
  | ThenKw -> "'then'"
  | ElseKw -> "'else'"
  | TrueKw -> "'true'"
  | FalseKw -> "'false'"
  | Ident name -> Printf.sprintf "identifier %S" name
  | Integer value -> Printf.sprintf "integer %d" value
  | End -> "end of input"

let begins_atom = function
  | Ident _ | Integer _ | TrueKw | FalseKw | LParen -> true
  | Lambda | Dot | Arrow | RParen | Plus | Minus | Star | Equal | EqualEqual
  | Lt | Le | Gt | Ge | And | Or
  | LetKw | RecKw | InKw | IfKw | ThenKw | ElseKw | End -> false

(*
   Precedence, weakest to strongest:

     let / let rec / if / fun
     && ||
     comparisons  (< <= > >= ==)
     + -
     *
     unary -
     application
     atoms
*)
let rec parse_expr stream =
  match (current stream).kind with
  | LetKw -> parse_let stream
  | IfKw -> parse_if stream
  | Lambda -> parse_fun stream
  | _ -> parse_logical stream

and parse_let stream =
  let let_token = advance stream in
  let recursive =
    match (current stream).kind with
    | RecKw ->
        ignore (advance stream);
        true
    | _ -> false
  in
  let name =
    match (current stream).kind with
    | Ident name ->
        ignore (advance stream);
        name
    | kind ->
        fail (current stream).position
          (Printf.sprintf "expected an identifier after 'let', found %s"
             (token_description kind))
  in
  (match (current stream).kind with
   | Equal -> ignore (advance stream)
   | kind ->
       fail (current stream).position
         (Printf.sprintf "expected '=' in let expression, found %s"
            (token_description kind)));
  let definition = parse_expr stream in
  (match (current stream).kind with
   | InKw -> ignore (advance stream)
   | kind ->
       fail let_token.position
         (Printf.sprintf "expected 'in' to complete let expression, found %s"
            (token_description kind)));
  let body = parse_expr stream in
  if recursive then LetRec (name, definition, body)
  else Let (name, definition, body)

and parse_if stream =
  ignore (advance stream);
  let condition = parse_expr stream in
  (match (current stream).kind with
   | ThenKw -> ignore (advance stream)
   | kind ->
       fail (current stream).position
         (Printf.sprintf "expected 'then', found %s" (token_description kind)));
  let then_branch = parse_expr stream in
  (match (current stream).kind with
   | ElseKw -> ignore (advance stream)
   | kind ->
       fail (current stream).position
         (Printf.sprintf "expected 'else', found %s" (token_description kind)));
  let else_branch = parse_expr stream in
  If (condition, then_branch, else_branch)

and parse_fun stream =
  let lambda_token = advance stream in
  let rec collect_parameters reversed =
    match (current stream).kind with
    | Ident name ->
        ignore (advance stream);
        collect_parameters (name :: reversed)
    | Dot | Arrow ->
        if reversed = [] then fail lambda_token.position "expected a parameter name";
        ignore (advance stream);
        List.rev reversed
    | kind ->
        fail (current stream).position
          (Printf.sprintf "expected a parameter or abstraction separator, found %s"
             (token_description kind))
  in
  let parameters = collect_parameters [] in
  let body = parse_expr stream in
  List.fold_right (fun parameter body -> Fun (parameter, body)) parameters body

and parse_logical stream =
  let first = parse_comparison stream in
  let rec loop accumulated =
    match (current stream).kind with
    | And ->
        ignore (advance stream);
        let right = parse_comparison stream in
        loop (Binop (And, accumulated, right))
    | Or ->
        ignore (advance stream);
        let right = parse_comparison stream in
        loop (Binop (Or, accumulated, right))
    | _ -> accumulated
  in
  loop first

and parse_comparison stream =
  let left = parse_additive stream in
  match (current stream).kind with
  | EqualEqual ->
      ignore (advance stream);
      Binop (Eq, left, parse_additive stream)
  | Lt ->
      ignore (advance stream);
      Binop (Lt, left, parse_additive stream)
  | Le ->
      ignore (advance stream);
      Binop (Le, left, parse_additive stream)
  | Gt ->
      ignore (advance stream);
      Binop (Gt, left, parse_additive stream)
  | Ge ->
      ignore (advance stream);
      Binop (Ge, left, parse_additive stream)
  | _ -> left

and parse_additive stream =
  let first = parse_multiplicative stream in
  let rec loop accumulated =
    match (current stream).kind with
    | Plus ->
        ignore (advance stream);
        let right = parse_multiplicative stream in
        loop (Binop (Add, accumulated, right))
    | Minus ->
        ignore (advance stream);
        let right = parse_multiplicative stream in
        loop (Binop (Sub, accumulated, right))
    | _ -> accumulated
  in
  loop first

and parse_multiplicative stream =
  let first = parse_unary stream in
  let rec loop accumulated =
    match (current stream).kind with
    | Star ->
        ignore (advance stream);
        let right = parse_unary stream in
        loop (Binop (Mul, accumulated, right))
    | _ -> accumulated
  in
  loop first

and parse_unary stream =
  match (current stream).kind with
  | Minus ->
      ignore (advance stream);
      Unop (Neg, parse_unary stream)
  | _ -> parse_application stream

and parse_application stream =
  let first = parse_atom stream in
  let rec loop accumulated =
    if begins_atom (current stream).kind then
      let argument = parse_atom stream in
      loop (App (accumulated, argument))
    else
      accumulated
  in
  loop first

and parse_atom stream =
  let token = current stream in
  match token.kind with
  | Ident name ->
      ignore (advance stream);
      Var name
  | Integer value ->
      ignore (advance stream);
      Int value
  | TrueKw ->
      ignore (advance stream);
      Bool true
  | FalseKw ->
      ignore (advance stream);
      Bool false
  | LParen ->
      ignore (advance stream);
      let expression = parse_expr stream in
      (match (current stream).kind with
       | RParen -> ignore (advance stream)
       | kind ->
           fail (current stream).position
             (Printf.sprintf "expected ')', found %s" (token_description kind)));
      expression
  | kind ->
      fail token.position
        (Printf.sprintf "expected an expression, found %s" (token_description kind))

let parse input =
  try
    let tokens = Array.of_list (tokenize input) in
    let stream = { tokens; cursor = 0 } in
    let expression = parse_expr stream in
    (match (current stream).kind with
     | End -> Ok expression
     | kind ->
         fail (current stream).position
           (Printf.sprintf "unexpected %s after complete expression"
              (token_description kind)))
  with
  | Internal_parse_error error -> Error error

let parse_exn input =
  match parse input with
  | Ok expression -> expression
  | Error error -> invalid_arg (string_of_error error)
