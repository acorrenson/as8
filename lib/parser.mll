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
let adr = dig dig* dig*
let cst = dig dig*
let lab =  ['a'-'z' 'A'-'Z' '_' '0'-'9']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let white = [' ' '\t']*
let line = '\n'
let word = dig dig* dig* dig*

rule instruction = parse

  | ';' [^ '\n']+ '\n' {
    next_line lexbuf;
    instruction lexbuf
  }

  | ';' [^ '\n']* '\n' {
    raise (SyntaxError "empty or trainling comment" )
  }

  | ';' [^ '\n']* eof {
    raise (SyntaxError "trainling comment" )
  }

  | lab as l white ':' {
    LABEL l
  }

  | "ADD" white (reg as x)  white ',' white (reg  as y) {
    ADD_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "ADD" white (reg as x)  white ',' white (cst as c) {
    ADD_V_C (int_of_hex x.[1], hex c)
  }

  | "ADD" white "I" white ',' white (reg as r) {
    ADD_I_V (int_of_hex r.[1])
  }

  | "SUB" white (reg as x) white ',' white (reg as y) {
    SUB_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "SUBN" (reg as x) white ',' white (reg as y) {
    SUBN_V_V (int_of_hex x.[1], int_of_hex y.[1])
  }

  | "JP" white (lab as l) {
    PJP_A l
  }

  | line  { next_line lexbuf; instruction lexbuf }
  | white { instruction lexbuf }

  | "CLS" {
    CLS
  }
  
  | "RET" {
    RET
  }
  
  | "SYS" white (adr as a) {
    SYS (hex a)
  }

  | "SYS" white (lab as l) {
    PSYS_A l
  }

  | "CALL" white (adr as a) {
    CALL_A (hex a)
  }

  | "CALL" white (lab as l) {
    PCALL_A l
  }
  
  | "JP" white (adr as a) {
    JP_A (hex a)
  }
  
  | "JP" "V0" (adr as a) {
    JP_V0_A (hex a)
  }

  | "JP" "V0" (lab as l) {
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

  | "LD" white "[I]" white ',' white (reg as r) {
    LD_II_V (int_of_hex r.[1])
  }

  | "LD"  white (reg as r) white ',' white "[I]" {
    LD_V_II (int_of_hex r.[1])
  }

  | "DRW" white (reg as r1) white ',' white (reg as r2) white ',' white (dig as n) {
    DRW_V_V_N (int_of_hex r1.[1], int_of_hex r2.[1], int_of_hex n)
  }

  | "DW" white (word as w) {
    DW (hex w)
  }
  

  
  (* 

  (* LOGIC *)
  | OR_V_V  of int * int
  | AND_V_V  of int * int
  | XOR_V_V  of int * int
  | SHR_V  of int
  | SHL_V  of int


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
    raise Eof
  }

  | _ as c {raise (SyntaxError ("unknow instruction" ^ (String.make 1 c) ))}


