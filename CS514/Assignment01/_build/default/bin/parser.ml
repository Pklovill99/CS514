exception Parse_error of string

type token_stream = {
  lexbuf : Lexing.lexbuf;
  mutable lookahead : Token.t;
}

let advance stream =
  stream.lookahead <- Lexer.token stream.lexbuf

(* Factor -> ( Expr ) | number *)
let rec parse_factor stream =
  match stream.lookahead with
  | Token.Int n ->
      advance stream;
      Ast.Int n
  | Token.LParen ->
      advance stream;

      let inner = parse_expr stream in

      (match stream.lookahead with
       | Token.RParen -> advance stream; inner
       | token ->
           raise
             (Parse_error
                (Printf.sprintf
                   "Expected ')' but found %s"
                   (Token.to_string token))))
  | token ->
      raise
        (Parse_error
           (Printf.sprintf
              "Expected a number or '(' but found %s"
              (Token.to_string token)))

(* Unary -> + Unary | - Unary | Factor *)
and parse_unary stream =
  match stream.lookahead with
  | Token.Plus ->
      advance stream;
      let operand = parse_unary stream in
      Ast.Pos operand
  | Token.Minus ->
      advance stream;
      let operand = parse_unary stream in
      Ast.Neg operand
  | _ ->
      parse_factor stream

(* Term -> Term * Unary | Term / Unary | Unary *)
and parse_term stream =
  let first = parse_unary stream in

  let rec parse_term' left =
    match stream.lookahead with
    | Token.Star ->
        advance stream;

        let right = parse_unary stream in
        let combined = Ast.Mul (left, right) in

        parse_term' combined
    | Token.Slash ->
        advance stream;

        let right = parse_unary stream in
        let combined = Ast.Div (left, right) in

        parse_term' combined
    | _ -> left
  in

  parse_term' first

(* Expr -> Expr + Term | Expr - Term | Term *)
and parse_expr stream =
  let first = parse_term stream in

  let rec parse_expr' left =
    match stream.lookahead with
    | Token.Plus ->
        advance stream;

        let right = parse_term stream in
        let combined = Ast.Add (left, right) in

        parse_expr' combined
    | Token.Minus ->
        advance stream;

        let right = parse_term stream in
        let combined = Ast.Sub (left, right) in

        parse_expr' combined
    | _ -> left
  in

  parse_expr' first

let parse lexbuf =
  let stream =
    {
      lexbuf;
      lookahead = Lexer.token lexbuf;
    }
  in

  let result = parse_expr stream in

  match stream.lookahead with
  | Token.Eof -> result
  | token ->
      raise
        (Parse_error
           (Printf.sprintf
              "Expected end of input but found %s"
              (Token.to_string token)))