open Instructions

let opcode_x_y base x y =
  base lor (x lsl 8) lor (y lsl 4)

let opcode_nnn base nnn =
  base lor nnn

let opcode_x_kk base x kk =
  base lor (x lsl 8) lor kk

let opcode_x base x =
  base lor x

let print_hex x = Printf.printf "%#04x\n" x

let process_standard_instruction adt i =
  match i with
  | CLS -> 0x00E0 |> print_hex
  | RET -> 0x00EE |> print_hex
  | SYS nnn -> (opcode_nnn 0 nnn) |> print_hex
  | CALL_A nnn -> (opcode_nnn 0x2000 nnn) |> print_hex

  (* JUMP *)
  | JP_A nnn -> (opcode_nnn 0x1000 nnn) |> print_hex
  | JP_V0_A nnn -> (opcode_nnn 0xB000 nnn) |> print_hex

  (* CONDITION *)
  | SE_V_C (x, kk) -> opcode_x_kk 0x1000 x kk |> print_hex
  | SE_V_V (x, y) -> opcode_x_y 0x5000 x y |> print_hex
  | SNE_V_C (x, kk) -> opcode_x_kk 0x4000 x kk |> print_hex
  | SNE_V_V (x, y) -> opcode_x_y 0x9000 x y |> print_hex
  | SKP_V x -> opcode_x 0xE09E x |> print_hex
  | SKNP_V x -> opcode_x 0xE0A1 x |> print_hex

  (* LOAD *)
  | LD_V_V (x, y) -> opcode_x_y 0x8000 x y |> print_hex
  | LD_V_C (x, kk) -> opcode_x_kk 0x6000 x kk |> print_hex
  | LD_I_A nnn -> opcode_nnn 0xA000 nnn |> print_hex
  | LD_V_DT x -> opcode_x 0xF007 x |> print_hex
  | LD_V_K x -> opcode_x 0xF00A x |> print_hex
  | LD_DT_V x -> opcode_x 0xF015 x |> print_hex
  | LD_ST_V x -> opcode_x 0xF018 x |> print_hex
  | LD_F_V x -> opcode_x 0xF029 x |> print_hex
  | LD_B_V x -> opcode_x 0xF033 x |> print_hex
  | LD_II_V x -> opcode_x 0xF055 x |> print_hex
  | LD_V_II x -> opcode_x 0xF055 x |> print_hex

  (* ARITH *)
  | ADD_V_C (x, kk) -> opcode_x_kk 0x7000 x kk |> print_hex
  | ADD_V_V (x, y) -> opcode_x_y 0x8004 x y |> print_hex
  | ADD_I_V x -> opcode_x 0xF01E x |> print_hex
  | SUB_V_V (x, y) -> opcode_x_y 0x8005 x y |> print_hex
  | SUBN_V_V (x, y) -> opcode_x_y 0x8007 x y |> print_hex

  (* LOGIC *)
  | OR_V_V (x, y) -> opcode_x_y 0x8001 x y |> print_hex
  | AND_V_V (x, y) -> opcode_x_y 0x8002 x y |> print_hex
  | XOR_V_V (x, y) -> opcode_x_y 0x8003 x y |> print_hex
  | SHR_V x -> opcode_x 0x8006 x |> print_hex
  | SHL_V x -> opcode_x 0x800E x |> print_hex

  (* PSEUDO *)
  | LABEL l -> print_endline l
  | PLD_I_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0xA000 nnn |> print_hex
      | None -> failwith "no label found"
    end
  | PCALL_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x2000 nnn |> print_hex
      | None -> failwith "no label found"
    end
  | PSYS_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x0000 nnn |> print_hex
      | None -> failwith "no label found"
    end
  | PJP_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x1000 nnn |> print_hex
      | None -> failwith "no label found"
    end
  | PJP_V0_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0xB000 nnn |> print_hex
      | None -> failwith "no label found"
    end
  | _ -> failwith "not yet supported"

(* extended *)

  (*
  | SCD of int
  | SCR of int
  | EXIT of int
  | LOW of int
  | HIGH of int
  | DRW_0 of int * int
  | LD_HF_V of int
  | LD_R_V of int
  | LD_V_R of int*)


