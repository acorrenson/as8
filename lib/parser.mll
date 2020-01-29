{
  open Lexing
  open Instructions
  exception SyntaxError of string
  exception Eof

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }

  let int_of_hex c =
    match c with
    | '0'..'9' -> (int_of_char c) -  48
    | 'A'..'Z' -> (int_of_char c) - 55
    | 'a'..'z' -> (int_of_char c) - 87
    | _ -> failwith ("impossible conversion")
}

let reg = 'V' ['0' - '9' 'A'-'F']
let lab =  ['a'-'z' 'A'-'Z' '-']['a'-'z' 'A'-'Z' '-']*
let white = [' ' '\t']*
let line = '\n'

rule instruction = parse

  | ';' [^ '\n']+ '\n' {
    next_line lexbuf;
    Printf.printf "COMMENT\n";
    instruction lexbuf
  }

  | ';' [^ '\n']* '\n' {
    raise (SyntaxError "empty or trainling comment" )
  }

  | ';' [^ '\n']* eof {
    raise (SyntaxError "trainling comment" )
  }

  | lab as l white ':' {
    Printf.printf "LABEL\n";
    LABEL l
  }

  | "ADD" white (reg as x)  white ',' white (reg  as y) {
    Printf.printf "ADD\n";
    ADD_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "SUB" (reg as x) white ',' white (reg as y) {
    Printf.printf "SUB\n";
    SUB_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "JP" white (lab as l) {
    print_endline "JP";
    PJP_A l
  }

  | line  { print_endline "nl"; next_line lexbuf; instruction lexbuf }
  | white { instruction lexbuf }


  | eof {
    print_endline "EOF";
    raise Eof
  }

  | _ as c {raise (SyntaxError ("unknow instruction" ^ (String.make 1 c) ))}


