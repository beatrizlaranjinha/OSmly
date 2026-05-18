(* Estrutura que representa uma entrada no plano de execução. *)
type plan_entry = {
  program_name : string;  (* Nome do programa *)
  arrival_time : int;     (* Tempo de chegada *)
  priority : int;         (* Prioridade de escalonamento *)
}

(* Processa uma linha de texto e converte numa estrutura plan_entry. *)
let parse_plan_line line =
  let tokens = String.split_on_char ' ' (String.trim line) |> List.filter (fun s -> s <> "") in
  match tokens with
  | [program_name; arrival_time; priority] ->
      (match int_of_string_opt arrival_time, int_of_string_opt priority with
       | Some a, Some p -> Some { program_name; arrival_time = a; priority = p }
       | _ -> None)
  | [program_name; arrival_time] ->
      (match int_of_string_opt arrival_time with
       | Some a -> Some { program_name; arrival_time = a; priority = 1 }
       | _ -> None)
  | [] -> None
  | _ -> None

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
  try
    let channel = open_in filename in
    let lines = read_lines channel in
    List.filter_map parse_plan_line lines
  with Sys_error _ ->
    print_endline ("Aviso: Ficheiro de plano " ^ filename ^ " não encontrado. O simulador arranca vazio.");
    []
