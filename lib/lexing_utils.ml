type debug_info = { pos:int; text:string }

type token =
  | SYM of debug_info * string
  | TAG of debug_info * string
  | DIR of debug_info * string
  | REG of debug_info * int
  | CST of debug_info * int
  | CMT of debug_info * string
  | COM of debug_info

type operand =
  | Reg of int
  | Cst of int
  | Tag of string

type source_line = {
  num : int;
  label : string option;
  mnemonic : string option;
  operands : operand list;
  comments : string list;
}

let print_pos outx p = Printf.fprintf outx "(pos:%d)[%s]" p.pos p.text


let print_token t =
  match t with
  | SYM (p, s) -> Printf.printf "SYM = %-10s %a\n" s print_pos p
  | CMT (p, s) -> Printf.printf "CMT = %-10s %a\n" s print_pos p
  | TAG (p, s) -> Printf.printf "TAG = %-10s %a\n" s print_pos p
  | COM _ -> Printf.printf ",\n"
  | DIR (p, s) -> Printf.printf "DIR = %-10s %a\n" s print_pos p
  | CST (p, i) -> Printf.printf "CST = %-10d %a\n" i print_pos p
  | REG (p, i) -> Printf.printf "REG = %-10d %a\n" i print_pos p


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