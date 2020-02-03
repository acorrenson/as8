open Cmdliner
open Lib_as8.Assembler

let input =
  let doc = "a CHIP8 assembly file" in
  let docv = "source.as8" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)

let output =
  let doc = "a CHIP8 rom file" in
  let docv = "dest.rom" in
  Arg.(required & pos 1 (some string) None & info [] ~doc ~docv)

let build file out =
  assembler file out

let cmd_run =
  let doc = "build a CHIP8 assembly file" in
  let man = [
    `S Manpage.s_authors;
    `P "Arthur Correnson <arthur.correnson@univ-tlse3.fr>";
    `S Manpage.s_bugs;
    `P "If you find a bug, please send an email to <arthur.correnson@univ-tlse3.fr>";
  ] in
  Term.(const build $ input $ output),
  Term.info "build" ~version:"v0.1" ~exits:Term.default_exits ~doc ~man

let cmd =
  let err = Term.(const (fun () -> `Error (true, "no command provided")) $ const ()) in
  Term.(ret err),
  Term.info "as8" ~version:"v0.1" ~exits:Term.default_exits

let _ =
  Term.(exit @@ eval_choice cmd [cmd_run])