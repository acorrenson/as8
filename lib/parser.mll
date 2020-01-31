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

  let explode s =
    let l = String.length s in
    let rec step i acc =
      if i = l then acc else step (i+1) (acc @ [s.[i]] )
    in
    step 0 []
  
  let hex s =
    explode s
    |> List.fold_left (fun h c -> (int_of_hex c) + 16 * h) 0

}

let dig = ['0' - '9' 'A'-'F']
let reg = 'V' dig
let adr = dig dig dig
let cst = dig dig
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

  | "ADD" white (reg as x)  white ',' white (cst as c) {
    Printf.printf "ADD\n";
    ADD_V_C (int_of_hex x.[1], hex c)
  }

  | "ADD" white "I" white ',' white (reg as r) {
    Printf.printf "ADD\n";
    ADD_I_V (int_of_hex r.[1])
  }

  | "SUB" (reg as x) white ',' white (reg as y) {
    Printf.printf "SUB\n";
    SUB_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "SUBN" (reg as x) white ',' white (reg as y) {
    Printf.printf "SUB\n";
    SUBN_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "JP" white (lab as l) {
    print_endline "JP";
    PJP_A l
  }

  | line  { print_endline "nl"; next_line lexbuf; instruction lexbuf }
  | white { instruction lexbuf }

  | "CLS" {
    Printf.printf "CLS\n";
    CLS
  }
  
  | "RET" {
    Printf.printf "RET\n";
    RET
  }
  
  | "SYS" white (adr as a) {
    Printf.printf "SYS\n";
    SYS (hex a)
  }

  | "SYS" white (lab as l) {
    Printf.printf "SYS\n";
    PSYS_A l
  }

  | "CALL" white (adr as a) {
    Printf.printf "SYS\n";
    CALL_A (hex a)
  }

  | "CALL" white (lab as l) {
    Printf.printf "SYS\n";
    PCALL_A l
  }
  
  | "JP" white (adr as a) {
    Printf.printf "JP\n";
    JP_A (hex a)
  }
  
  | "JP" "V0" (adr as a) {
    Printf.printf "JP\n";
    JP_V0_A (hex a)
  }

  | "JP" "V0" (lab as l) {
    Printf.printf "JP\n";
    PJP_V0_A l
  }
  
  | "SE" white (reg as r) white ',' white (cst as c) {
    SE_V_C ( int_of_hex r.[1], hex c )
  }

  | "SE" white (reg as r1) white ',' white (reg as r2) {
    SE_V_V ( int_of_hex r1.[1], int_of_hex r2.[1] )
  }

  | "SNE" white (reg as r) white ',' white (cst as c) {
    SNE_V_C ( int_of_hex r.[1], hex c )
  }
  
  | "SNE" white (reg as r1) white ',' white (reg as r2) {
    SNE_V_V  ( int_of_hex r1.[1], int_of_hex r2.[1] )
  }
  
  | "SKP" white (reg as r) {
    SKP_V ( int_of_hex r.[1] )
  }

  | "SKNP" white (reg as r) {
    SKNP_V ( int_of_hex r.[1] )
  }
  
  | "LD" white (reg as r1) white ',' white (reg as r2) {
    LD_V_V (int_of_hex r1.[1], int_of_hex r2.[1])
  }
  
  | "LD" white (reg as r) white ',' white (cst as c) {
    LD_V_C  (int_of_hex r.[1], hex c)
  }

  | "LD" white "I" white ',' white (adr as a) {
    LD_I_A (hex a)
  }

  | "LD" white "I" white ',' white (lab as l) {
    PLD_I_A l
  }
  
  
  | "LD" white (reg as r) ',' white "DT" {
    LD_V_DT (int_of_hex r.[1])
  }
  
  | "LD" white (reg as r) ',' white "K" {
    LD_V_K (int_of_hex r.[1])
  }

  | "LD" white "DT" ',' white (reg as r) {
    LD_DT_V (int_of_hex r.[1])
  }

  | "LD" white "ST" ',' white (reg as r) {
    LD_ST_V (int_of_hex r.[1])
  }
  

  | "LD" white "F" ',' white (reg as r) {
    LD_F_V (int_of_hex r.[1])
  }

  | "LD" white "B" ',' white (reg as r) {
    LD_B_V (int_of_hex r.[1])
  }

  | "LD" white "[I]" ',' white (reg as r) {
    LD_II_V (int_of_hex r.[1])
  }

  | "LD"  white (reg as r) ',' white "[I]" {
    LD_V_II (int_of_hex r.[1])
  }
  
  (* 

  (* LOGIC *)
  | OR_V_V  of int * int
  | AND_V_V  of int * int
  | XOR_V_V  of int * int
  | SHR_V  of int
  | SHL_V  of int

  (* PSEUDO *)
  | LABEL     of string
  | PLD_I_A   of string
  | PCALL_A   of string
  | PSYS_A    of string
  | PJP_A     of string
  | PJP_V0_A   of string

  (* extended *)
  | SCD of int
  | SCR of int
  | EXIT of int
  | LOW of int
  | HIGH of int
  | DRW_0 of int * int
  | LD_HF_V of int
  | LD_R_V of int
  | LD_V_R of int
  *)

  | eof {
    print_endline "EOF";
    raise Eof
  }

  | _ as c {raise (SyntaxError ("unknow instruction" ^ (String.make 1 c) ))}


