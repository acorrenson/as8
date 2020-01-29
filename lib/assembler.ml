open Parser
open Instructions_processing
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let assembler file =
  let inx = open_in file in
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
  List.iter (process_standard_instruction adresses_table) !instructions

