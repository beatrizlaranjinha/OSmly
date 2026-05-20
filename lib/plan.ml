type plan_entry = {
  program_name : string;
  arrival_time : int;
  priority : int;
}

let default_priority = 1

let parse_plan_line line =
  match String.split_on_char ' ' (String.trim line) with
  | [program_name; arrival] ->
      {
        program_name;
        arrival_time = int_of_string arrival;
        priority = default_priority;
      }

  | [program_name; arrival; priority] ->
      {
        program_name;
        arrival_time = int_of_string arrival;
        priority = int_of_string priority;
      }

  | _ ->
      failwith ("Invalid plan line: " ^ line)

let read_file_lines filename =
  let ic = open_in filename in

  let rec loop acc =
    match input_line ic with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in ic;
        List.rev acc
  in

  loop []

let load_plan filename =
  filename
  |> read_file_lines
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map parse_plan_line

let arrivals_at_time time plan =
  List.filter
    (fun entry -> entry.arrival_time = time)
    plan

let remove_arrivals_at_time time plan =
  List.filter
    (fun entry -> entry.arrival_time <> time)
    plan

let print_plan plan =
  List.iter
    (fun entry ->
      Printf.printf
        "Program=%s Arrival=%d Priority=%d\n"
        entry.program_name
        entry.arrival_time
        entry.priority)
    plan
