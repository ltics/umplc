open Syntax

let string_of_pos pos =
  let open Lexing in
  if String.length pos.pos_fname > 0 then
    Format.sprintf "%s, line %d, column %d" pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
  else
    Format.sprintf "line %d, column %d" pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)

let parse_from_lexbuf (lexbuf : Lexing.lexbuf) : exp =
    let open Lexing in
    let open Format in
    try
      Parser.program Lexer.token lexbuf
    with
    | Failure "lexing: empty token" ->
      failwith (sprintf "lexical error at %s"
                      (string_of_pos lexbuf.lex_curr_p))
    | Parsing.Parse_error ->
      failwith (sprintf "parse error at %s; unexpected token %s"
                      (string_of_pos lexbuf.lex_curr_p)
                      (lexeme lexbuf))

let from_file (filename : string) : exp =
  let open Lexing in
  let lexbuf = from_channel (open_in filename) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_from_lexbuf lexbuf

let from_string (str : string) : exp =
  let open Lexing in
  let lexbuf = from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "" };
  parse_from_lexbuf lexbuf