{
  open Lexing
  open Lexing_utils

  exception Eof

  let pos lexbuf = {
    pos = lexeme_start lexbuf;
    text = lexeme lexbuf;
  }
}

let hex_dig = ['0' - '9' 'A'-'F']
let dig = ['0' - '9']
let reg = 'V' dig
let cst = dig dig*
let hex_cst = hex_dig hex_dig*
let sym =  ['a'-'z' 'A'-'Z' '_' '0'-'9']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let col = ':'
let white = [' ' '\t']+

rule token = parse
  | white { token lexbuf }

  | 'V' (hex_cst as h) {
    REG (pos lexbuf, hex h)
  }

  | ',' {
    COM (pos lexbuf)
  }

  | (sym as s) ':' {
    TAG (pos lexbuf, s)
  }

  | sym as s {
    SYM (pos lexbuf, s)
  }

  | '#' hex_cst as h {
    CST (pos lexbuf, hex h)
  }

  | cst as c {
    CST (pos lexbuf, int_of_string c)
  }

  | ';' ([^ '\n']* as txt) eof {
    CMT (pos lexbuf, txt)
  }

  | eof {
    raise Eof
  }

