{
  open Lexing
  open Impparser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos;
                                    pos_lnum = pos.pos_lnum + 1 }
}

let intPattern = ['0'-'9']+
let white = [' ' '\t']+
let vars = ['a'-'z']+
let newline = '\n'

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | intPattern { INT(int_of_string(Lexing.lexeme lexbuf)) }
  | vars { read lexbuf }
  | '+' { PLUS }
  | '-' { MINUS }
  | '<' { LT }
  | '=' { EQ }
  | "<=" { LEQ }
  | "&&" { AND }
  | "||" { OR }
  | '!' { NOT }
  | "output" { OUTPUT }
  | "while" { WHILE }
  | "skip" { SKIP }
  | "seq" { SEQ }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { raise (SyntaxError("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
