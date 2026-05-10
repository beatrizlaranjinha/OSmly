open Instructions

let memory_size = 1000

(* Memória simulada: lista de instruções. *)
type memory = instruction list

(* Cria memória vazia com 1000 posições Empty. *)
let create_memory () =
  List.init memory_size (fun _ -> Empty)

(* Lê todas as linhas de um ficheiro de forma recursiva. *)
let rec read_lines channel =
  try
    let line = input_line channel in
    line :: read_lines channel
  with End_of_file ->
    close_in channel;
    []

(* Lê um ficheiro .prg e converte cada linha numa instruction. *)
let load_program_from_file filename =
  let channel = open_in filename in
  read_lines channel
  |> List.map parse_instruction

(* Substitui elementos de uma lista a partir de uma posição. *)
let rec replace_at memory program index =
  match memory, program with
  | [], _ -> []
  | memory, [] -> memory
  | head_memory :: tail_memory, head_program :: tail_program ->
      if index = 0 then
        head_program :: replace_at tail_memory tail_program 0
      else
        head_memory :: replace_at tail_memory program (index - 1)

(* Carrega um programa para a memória numa posição inicial. *)
let load_program memory program start_address =
  replace_at memory program start_address
