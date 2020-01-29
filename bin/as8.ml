open Cmdliner

let input =
  let doc = "a CHIP8 assembly file" in
  let docv = "source.as8" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

let f =
  let doc = "a flag" in 
  let docv = "a flag" in
  Arg.(value & flag & info ["f"] ~doc ~docv)

let run file f =
  print_endline file;
  if f then print_endline "f"

let cmd_run =
  let doc = "Assembler for CHIP8 assembly language" in
  let man = [
    `S Manpage.s_authors;
    `P "Arthur Correnson <arthur.correnson@univ-tlse3.fr>";
    `S Manpage.s_bugs;
    `P "If you find a bug, please send an email to <arthur.correnson@univ-tlse3.fr>";
  ] in
  Term.(const run $ input $ f),
  Term.info "as8" ~version:"v0.1" ~exits:Term.default_exits ~doc ~man

let _ =
  Term.(exit @@ eval cmd_run)