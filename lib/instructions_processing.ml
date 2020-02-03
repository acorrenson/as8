open Instructions

let opcode_x_y base x y =
  base lor (x lsl 8) lor (y lsl 4)

let opcode_x_y_n base x y n =
  base lor (x lsl 8) lor (y lsl 4) lor n

let opcode_nnn base nnn =
  base lor nnn

let opcode_x_kk base x kk =
  base lor (x lsl 8) lor kk

let opcode_x base x =
  base lor (x lsl 8)

let print_hex x = Printf.printf "%#04x\n" x
let write_char oux x =
  output_char oux (char_of_int (x lsr 8));
  output_char oux (char_of_int (x land 0x00FF))


let process_standard_instruction adt oux i =
  match i with
  | CLS -> 0x00E0 |> write_char oux
  | RET -> 0x00EE |> write_char oux
  | SYS nnn -> (opcode_nnn 0 nnn) |> write_char oux
  | CALL_A nnn -> (opcode_nnn 0x2000 nnn) |> write_char oux

  (* JUMP *)
  | JP_A nnn -> (opcode_nnn 0x1000 nnn) |> write_char oux
  | JP_V0_A nnn -> (opcode_nnn 0xB000 nnn) |> write_char oux

  (* CONDITION *)
  | SE_V_C (x, kk) -> opcode_x_kk 0x3000 x kk |> write_char oux
  | SE_V_V (x, y) -> opcode_x_y 0x5000 x y |> write_char oux
  | SNE_V_C (x, kk) -> opcode_x_kk 0x4000 x kk |> write_char oux
  | SNE_V_V (x, y) -> opcode_x_y 0x9000 x y |> write_char oux
  | SKP_V x -> opcode_x 0xE09E x |> write_char oux
  | SKNP_V x -> opcode_x 0xE0A1 x |> write_char oux

  (* LOAD *)
  | LD_V_V (x, y) -> opcode_x_y 0x8000 x y |> write_char oux
  | LD_V_C (x, kk) -> opcode_x_kk 0x6000 x kk |> write_char oux
  | LD_I_A nnn -> opcode_nnn 0xA000 nnn |> write_char oux
  | LD_V_DT x -> opcode_x 0xF007 x |> write_char oux
  | LD_V_K x -> opcode_x 0xF00A x |> write_char oux
  | LD_DT_V x -> opcode_x 0xF015 x |> write_char oux
  | LD_ST_V x -> opcode_x 0xF018 x |> write_char oux
  | LD_F_V x -> opcode_x 0xF029 x |> write_char oux
  | LD_B_V x -> opcode_x 0xF033 x |> write_char oux
  | LD_II_V x -> opcode_x 0xF055 x |> write_char oux
  | LD_V_II x -> opcode_x 0xF055 x |> write_char oux

  (* ARITH *)
  | ADD_V_C (x, kk) -> opcode_x_kk 0x7000 x kk |> write_char oux
  | ADD_V_V (x, y) -> opcode_x_y 0x8004 x y |> write_char oux
  | ADD_I_V x -> opcode_x 0xF01E x |> write_char oux
  | SUB_V_V (x, y) -> opcode_x_y 0x8005 x y |> write_char oux
  | SUBN_V_V (x, y) -> opcode_x_y 0x8007 x y |> write_char oux

  (* LOGIC *)
  | OR_V_V (x, y) -> opcode_x_y 0x8001 x y |> write_char oux
  | AND_V_V (x, y) -> opcode_x_y 0x8002 x y |> write_char oux
  | XOR_V_V (x, y) -> opcode_x_y 0x8003 x y |> write_char oux
  | SHR_V x -> opcode_x 0x8006 x |> write_char oux
  | SHL_V x -> opcode_x 0x800E x |> write_char oux

  | DRW_V_V_N (x, y, n) -> opcode_x_y_n 0xD000 x y n |> write_char oux
  | DW n -> write_char oux n

  (* PSEUDO *)
  | LABEL _ -> ()
  | PLD_I_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0xA000 nnn |> write_char oux
      | None -> failwith ("[LD I A] no label found " ^ l)
    end
  | PCALL_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x2000 nnn |> write_char oux
      | None -> failwith ("[CALL A] no label found " ^ l)
    end
  | PSYS_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x0000 nnn |> write_char oux
      | None -> failwith ("[SYS A] no label found " ^ l)
    end
  | PJP_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0x1000 nnn |> write_char oux
      | None -> failwith ("[JP A] no label found " ^ l)
    end
  | PJP_V0_A l ->
    begin
      match List.assoc_opt l adt with
      | Some nnn -> opcode_nnn 0xB000 nnn |> write_char oux
      | None -> failwith ("[JP V0 A] no label found " ^ l)
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


