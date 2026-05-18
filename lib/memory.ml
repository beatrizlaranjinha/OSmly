open Instructions

(* Dimensão total da memória. *)
let memory_size = 1000

(* Tipo de dados para representar a memória. *)
type memory = instruction array

(* Inicializa um array de memória com instruções vazias. *)
let create_memory () =
  Array.make memory_size Empty

(* Procura um bloco contíguo de tamanho livre na memória (First Fit). *)
let allocate_memory allocated_blocks size =
  let sorted_blocks = List.sort (fun (a,_) (b,_) -> compare a b) allocated_blocks in
  let rec find_hole current_pos blocks =
    if current_pos + size <= memory_size then
      match blocks with
      | [] -> Some current_pos
      | (b_start, b_size) :: rest ->
          if current_pos + size <= b_start then
            Some current_pos
          else
            find_hole (max current_pos (b_start + b_size)) rest
    else None
  in
  if size <= 0 then Some 0 else find_hole 0 sorted_blocks

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

(* Lê um ficheiro e converte as suas linhas numa lista de instruções.
   O enunciado exige que a 1ª linha especifique o tamanho inicial da memória. *)
let load_program_from_file filename =
  let channel = open_in filename in
  let lines = read_lines channel in
  match lines with
  | [] -> (0, [])
  | first_line :: rest ->
      (match int_of_string_opt (String.trim first_line) with
       | Some size -> 
           (* Proteção contra tamanhos negativos inseridos por utilizadores maliciosos *)
           let safe_size = if size < 0 then 0 else size in
           (safe_size, List.map parse_instruction rest)
       | None -> 
           (* Fallback: se não for número, assume o tamanho das instruções *)
           (List.length lines, List.map parse_instruction lines))

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
