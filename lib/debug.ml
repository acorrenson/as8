
open Lib_as8.Lexer
open Lib_as8.Lexing_utils

let lexbuf = Lexing.from_string "label: ADD V0, V1 ; comment"

let _ =
  while true do
    try print_token (token lexbuf)
    with Eof -> print_endline "EOF"; exit(0)
  done

