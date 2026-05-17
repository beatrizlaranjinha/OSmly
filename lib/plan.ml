(* Estrutura que representa uma entrada no plano de execução. *)
type plan_entry = {
  program_name : string;  (* Nome do programa *)
  arrival_time : int;     (* Tempo de chegada *)
  priority : int;         (* Prioridade de escalonamento *)
}

(* Processa uma linha de texto e converte numa estrutura plan_entry. *)
let parse_plan_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [program_name; arrival_time; priority] ->
      {
        program_name;
        arrival_time = int_of_string arrival_time;
        priority = int_of_string priority;
      }
  | [program_name; arrival_time] ->
      {
        program_name;
        arrival_time = int_of_string arrival_time;
        priority = 1; (* Prioridade por defeito *)
      }
  | _ ->
      failwith "Linha inválida no ficheiro plan.txt"

(* Lê todas as linhas de um canal de entrada. *)
let read_lines channel =
  let rec loop acc =
    try
      let line = input_line channel in
      loop (line :: acc)
    with End_of_file ->
      close_in channel;
      List.rev acc
  in
  loop []

(* Carrega o plano de execução a partir de um ficheiro. *)
let load_plan filename =
  let channel = open_in filename in
  read_lines channel
  |> List.map parse_plan_line
