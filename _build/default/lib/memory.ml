open Instructions

(* Dimensão total da memória. *)
let memory_size = 1000

(* Tipo de dados para representar a memória. *)
type memory = instruction array

(* Inicializa um array de memória com instruções vazias. *)
let create_memory () =
  Array.make memory_size Empty

(* Procura um bloco contíguo de tamanho livre na memória (First Fit). *)
let allocate_memory memory size =
  let rec search idx current_free start_free =
    if current_free = size then Some start_free
    else if idx >= memory_size then None
    else
      match memory.(idx) with
      | Instructions.Empty -> search (idx + 1) (current_free + 1) start_free
      | _ -> search (idx + 1) 0 (idx + 1)
  in
  if size <= 0 then Some 0 else search 0 0 0

(* Liberta a memória substituindo as instruções por Empty. *)
let free_memory memory start_address size =
  let new_memory = Array.copy memory in
  for i = 0 to size - 1 do
    if start_address + i < memory_size then
      new_memory.(start_address + i) <- Instructions.Empty
  done;
  new_memory

(* Lê todas as linhas de um ficheiro de forma recursiva. *)
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

(* Lê um ficheiro e converte as suas linhas numa lista de instruções. *)
let load_program_from_file filename =
  let channel = open_in filename in
  read_lines channel
  |> List.map parse_instruction

(* Carrega uma lista de instruções para a memória a partir de um endereço inicial.
   Retorna uma cópia da memória atualizada. *)
let load_program memory program start_address =
  let new_memory = Array.copy memory in
  List.iteri
    (fun i instr ->
      if start_address + i < memory_size then
        new_memory.(start_address + i) <- instr)
    program;
  new_memory

(* Imprime o conteúdo da memória no standard output. *)
let print_memory memory =
  Array.iteri
    (fun index instruction ->
      print_endline
        (string_of_int index ^ ": " ^
         Instructions.string_of_instruction instruction))
    memory
