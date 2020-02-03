(** Type for the standard set of CHIP8 instructions *)
type standard_set =
  (* SYSTEM *)
  | CLS
  | RET
  | SYS      of int
  | CALL_A   of int

  (* JUMP *)
  | JP_A     of int
  | JP_V0_A   of int

  (* CONDITION *)
  | SE_V_C   of int * int
  | SE_V_V   of int * int
  | SNE_V_C  of int * int
  | SNE_V_V  of int * int
  | SKP_V    of int
  | SKNP_V   of int


  (* LOAD *)
  | LD_V_V   of int * int
  | LD_V_C   of int * int
  | LD_I_A   of int
  | LD_V_DT  of int
  | LD_V_K   of int
  | LD_DT_V  of int
  | LD_ST_V  of int
  | LD_F_V   of int
  | LD_B_V   of int
  | LD_II_V  of int
  | LD_V_II  of int

  (* ARITH *)
  | ADD_V_C    of int * int
  | ADD_V_V    of int * int
  | ADD_I_V    of int
  | SUB_V_V    of int * int
  | SUBN_V_V   of int * int

  (* LOGIC *)
  | OR_V_V  of int * int
  | AND_V_V  of int * int
  | XOR_V_V  of int * int
  | SHR_V  of int
  | SHL_V  of int

  (* GRAPHIC *)
  | DRW_V_V_N of int * int * int

  (* PSEUDO *)
  | LABEL     of string
  | PLD_I_A   of string
  | PCALL_A   of string
  | PSYS_A    of string
  | PJP_A     of string
  | PJP_V0_A   of string
  | DW of int

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


exception OutOfBound of string

let adress i =
  if 0 <= i && i <= 0xFFF
  then i
  else raise (OutOfBound ("Adress " ^ (string_of_int i) ^ " is out of bound"))

let constant i =
  if 0 <= i && i <= 0xFF
  then i
  else raise (OutOfBound ("Constant " ^ (string_of_int i) ^ " is out of bound"))
