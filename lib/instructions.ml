type instruction =
  | M of int
  | A of int
  | S of int
  | B
  | T
  | C of int
  | L of string
  | Empty

let parse_instruction line =
  let line = String.trim line in
  if line = "" then Empty
  else
    match String.split_on_char ' ' line with
    | ["M"; n] -> M (int_of_string n)
    | ["A"; n] -> A (int_of_string n)
    | ["S"; n] -> S (int_of_string n)
    | ["B"] -> B
    | ["T"] -> T
    | ["C"; n] -> C (int_of_string n)
    | ["L"; filename] -> L filename
    | _ -> failwith ("Invalid instruction: " ^ line)

let string_of_instruction instr =
  match instr with
  | M n -> "M " ^ string_of_int n
  | A n -> "A " ^ string_of_int n
  | S n -> "S " ^ string_of_int n
  | B -> "B"
  | T -> "T"
  | C n -> "C " ^ string_of_int n
  | L filename -> "L " ^ filename
  | Empty -> "Empty"
