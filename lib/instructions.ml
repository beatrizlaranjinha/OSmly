(* Converte linhas dos ficheiros .prg em instruções OCaml. *)

type instruction =
  | M of int        (* muda a variável para n *)
  | A of int        (* adiciona n à variável *)
  | S of int        (* subtrai n à variável *)
  | B               (* bloqueia o processo *)
  | T               (* termina o processo *)
  | C of int        (* cria filho; pai salta n instruções *)
  | L of string     (* carrega/substitui por outro programa *)
  | Empty           (* instrução vazia/inválida *)

(* Transforma uma linha de texto numa instruction. *)
let parse_instruction line =
  match String.split_on_char ' ' (String.trim line) with
  | ["M"; n] -> M (int_of_string n)
  | ["A"; n] -> A (int_of_string n)
  | ["S"; n] -> S (int_of_string n)
  | ["B"] -> B
  | ["T"] -> T
  | ["C"; n] -> C (int_of_string n)
  | ["L"; filename] -> L filename
  | _ -> Empty

(* Transforma uma instruction em texto. *)
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
