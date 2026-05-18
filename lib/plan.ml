type plan_entry = {
  program_name : string;
  arrival_time : int;
  priority: int;
}

let parse_plan_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [program_name; arrival_time; priority] ->
      {
        program_name;
        arrival_time = int_of_string arrival_time;
        priority = int_of_string priority
      }
  | _ ->
      failwith "Linha inválida no plan.txt"

let rec read_lines channel =
  try
    let line = input_line channel in
    line :: read_lines channel
  with End_of_file ->
    close_in channel;
    []

let load_plan filename =
  let channel = open_in filename in
  read_lines channel
  |> List.map parse_plan_line
