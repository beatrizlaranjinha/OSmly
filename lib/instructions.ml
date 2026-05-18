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
  let tokens = String.split_on_char ' ' (String.trim line) |> List.filter (fun s -> s <> "") in
  match tokens with
  | ["M"; n] -> (match int_of_string_opt n with Some v -> M v | None -> Empty)
  | ["A"; n] -> (match int_of_string_opt n with Some v -> A v | None -> Empty)
  | ["S"; n] -> (match int_of_string_opt n with Some v -> S v | None -> Empty)
  | ["B"] -> B
  | ["T"] -> T
  | ["C"; n] -> (match int_of_string_opt n with Some v -> C v | None -> Empty)
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
