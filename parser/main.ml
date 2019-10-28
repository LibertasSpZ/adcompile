
open Printf
open Ast

let parse_file f  =
  let lexbuf = Lexing.from_channel (open_in f) in
  Adparser.program Adlexer.token lexbuf

