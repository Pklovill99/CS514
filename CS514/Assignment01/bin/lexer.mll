{
open Token

exception Lexing_error of string

let error_message lexbuf character =
  let position = Lexing.lexeme_start_p lexbuf in

  Printf.sprintf
    "Unexpected character '%c' at line %d, column %d"
    character
    position.pos_lnum
    (position.pos_cnum - position.pos_bol + 1)
}

let digit = ['0'-'9']
let integer = digit+
let horizontal_whitespace = [' ' '\t']+

rule token = parse
  | horizontal_whitespace
      { token lexbuf }

  | "\r\n"
      {
        Lexing.new_line lexbuf;
        token lexbuf
      }

  | '\n' | '\r'
      {
        Lexing.new_line lexbuf;
        token lexbuf
      }

  | integer as text
      { Int (int_of_string text) }

  | '+'
      { Plus }

  | '-'
      { Minus }

  | '*'
      { Star }

  | '/'
      { Slash }

  | '('
      { LParen }

  | ')'
      { RParen }

  | eof
      { Eof }

  | _ as character
      {
        raise
          (Lexing_error
             (error_message lexbuf character))
      }
