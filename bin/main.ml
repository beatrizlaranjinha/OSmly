open Group_project

let parse_algorithm arg =
  match String.lowercase_ascii arg with
  | "fcfs" -> Scheduler.FCFS
  | "priority" -> Scheduler.Priority
  | "sjfs" -> Scheduler.SJFS
  | _ ->
      Printf.printf "Algoritmo inválido. A usar FCFS.\n";
      Scheduler.FCFS

let rec run_control manager commands =
  match commands with
  | [] ->
      Report.report manager;
      Report.final_statistics manager

  | command :: rest -> (
      match String.trim command with
      | "E" ->
          Manager.execute manager;
          run_control manager rest

      | "D" ->
          Manager.unblock_one manager;
          run_control manager rest

      | "R" ->
          Report.report manager;
          run_control manager rest

      | "T" ->
          Printf.printf "\nSimulador terminado.\n";
          Report.report manager;
          Report.final_statistics manager

      | "" ->
          run_control manager rest

      | other ->
          Printf.printf "Comando inválido: %s\n" other;
          run_control manager rest
    )

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

let () =
  let algorithm =
    if Array.length Sys.argv > 1 then
      parse_algorithm Sys.argv.(1)
    else
      Scheduler.FCFS
  in

  let manager =
    Manager.create_manager
      "data/plan.txt"
      algorithm
      3
  in

  let commands =
    read_file_lines "data/control.txt"
  in

  run_control manager commands
