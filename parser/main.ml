
open Printf
open Ast
open Compiler

let parse_file f  =
  let lexbuf = Lexing.from_channel (open_in f) in
  Adparser.program Adlexer.token lexbuf

