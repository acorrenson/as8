{
  exception Eof
}

rule instruction = parse
  | _ { raise Eof }