open Parser
open Lexing
open Instructions_processing
open Instructions

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let compute_addresses il =
  let rec step l pc adt =
    match l with
    | [] -> adt
    | (LABEL l)::r ->
      begin
        match List.find_opt (fun (l', _) -> l' = l) adt with
        | Some _ -> failwith "overwriting label"
        | None -> step r pc ((l, pc)::adt)
      end
    | _::r -> step r (pc + 2) adt
  in
  step il 0x200 []

let print_table adt =
  List.iter (fun (x, i) -> Printf.printf "%s - %d\n" x i) adt

let assembler file out =
  let inx = open_in file in
  let oux = open_out_bin out in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };

  let instructions = ref [] in
  let end_of_parsing = ref false in

  while not !end_of_parsing do
    try instructions := !instructions @ [instruction lexbuf]
    with
    | Eof -> end_of_parsing := true
    | SyntaxError msg ->
      Printf.fprintf stderr "%a : %s \n" print_position lexbuf msg;
      exit(1)
  done;

  (* PASS - 1 *)
  let adresses_table = compute_addresses !instructions in
  (* PASS - 2 *)
  List.iter (process_standard_instruction adresses_table oux) !instructions;
  print_table adresses_table;

  close_in inx;
  close_out oux

