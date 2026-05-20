open Instructions

let memory_size = 1000

type memory = instruction array

let create_memory () =
  Array.make memory_size Empty

let read_file_lines filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      loop (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []

let load_program_from_file filename =
  filename
  |> read_file_lines
  |> List.map parse_instruction

let find_free_block memory program_size =
  let rec check_block start offset =
    if offset = program_size then true
    else if start + offset >= memory_size then false
    else
      match memory.(start + offset) with
      | Empty -> check_block start (offset + 1)
      | _ -> false
  in

  let rec search start =
    if start + program_size > memory_size then None
    else if check_block start 0 then Some start
    else search (start + 1)
  in

  search 0

let load_program memory program =
  let program_size = List.length program in
  match find_free_block memory program_size with
  | None -> failwith "Not enough memory to load program"
  | Some start ->
      List.iteri
        (fun i instr ->
          memory.(start + i) <- instr)
        program;
      start

let get_instruction memory pc =
  if pc < 0 || pc >= memory_size then Empty
  else memory.(pc)

let clear_program memory start size =
  for i = start to start + size - 1 do
    if i >= 0 && i < memory_size then
      memory.(i) <- Empty
  done

let print_memory memory =
  Array.iteri
    (fun i instr ->
      match instr with
      | Empty -> ()
      | _ ->
          Printf.printf "Memory[%d] = %s\n"
            i
            (string_of_instruction instr))
    memory
