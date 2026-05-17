(* Tipos de instruções suportadas pelo simulador. *)
type instruction =
  | M of int        (* Altera o valor da variável do processo para n *)
  | A of int        (* Adiciona n à variável do processo *)
  | S of int        (* Subtrai n à variável do processo *)
  | B               (* Bloqueia o processo *)
  | T               (* Termina o processo *)
  | C of int        (* Cria um processo filho. O pai salta n instruções. *)
  | L of string     (* Carrega um novo programa para a memória *)
  | Empty           (* Instrução vazia ou não reconhecida *)

(* Converte uma string numa estrutura instruction. *)
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

(* Converte uma estrutura instruction numa string representativa. *)
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
