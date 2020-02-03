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

let print_token t =
  match t with
  | SYM (_, s) -> print_endline ("SYM = " ^ s)
  | CMT (_, s) -> print_endline ("CMT = " ^ s)
  | TAG (_, s) -> print_endline ("TAG = " ^ s)
  | COM _ -> print_endline ","
  | DIR (_, s) -> print_endline ("DIR = " ^ s)
  | CST (_, i) -> print_endline ("CST = " ^ (string_of_int i))
  | REG (_, i) -> print_endline ("REG = " ^ (string_of_int i))


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