open Process
open Manager

let print_process process =
  Printf.printf
    "PID=%d | PPID=%d | Program=%s | PC=%d | Value=%d | Priority=%d | Period=%d | Deadline=%d | Arrival=%d | CPU=%d | State=%s\n"
    process.pid
    process.ppid
    process.program_name
    process.pc
    process.value
    process.priority
    process.period
    process.deadline
    process.arrival_time
    process.cpu_time
    (string_of_state process.state)

let print_process_list title processes =
  Printf.printf "\n%s\n" title;
  match processes with
  | [] -> Printf.printf "  none\n"
  | _ -> List.iter print_process processes

let print_running running =
  Printf.printf "\nPROCESSO EM EXECUÇÃO\n";
  match running with
  | None -> Printf.printf "  none\n"
  | Some process -> print_process process

let turnaround_time process =
  match process.end_time with
  | None -> None
  | Some end_time -> Some (end_time - process.arrival_time)

let waiting_time process =
  match turnaround_time process with
  | None -> None
  | Some turnaround -> Some (turnaround - process.cpu_time)

let print_finished_stats process =
  match turnaround_time process, waiting_time process, process.end_time with
  | Some turnaround, Some waiting, Some end_time ->
      Printf.printf
        "PID=%d | Program=%s | End=%d | CPU=%d | Turnaround=%d | Waiting=%d\n"
        process.pid
        process.program_name
        end_time
        process.cpu_time
        turnaround
        waiting
  | _ -> ()

let final_statistics manager =
  Printf.printf "\n========== ESTATÍSTICAS FINAIS ==========\n";

  match manager.terminated with
  | [] ->
      Printf.printf "Nenhum processo terminou.\n"

  | terminated ->
      List.iter print_finished_stats terminated;

      let total_turnaround =
        terminated
        |> List.filter_map turnaround_time
        |> List.fold_left ( + ) 0
      in

      let total_waiting =
        terminated
        |> List.filter_map waiting_time
        |> List.fold_left ( + ) 0
      in

      let n = List.length terminated in

      Printf.printf "\nMédia Turnaround: %.2f\n"
        (float_of_int total_turnaround /. float_of_int n);

      Printf.printf "Média Waiting: %.2f\n"
        (float_of_int total_waiting /. float_of_int n);

      Printf.printf "Total CPU usado: %d\n"
        (List.fold_left (fun acc p -> acc + p.cpu_time) 0 terminated);

  Printf.printf "=========================================\n"

let report manager =
  Printf.printf "\n==============================\n";
  Printf.printf "TEMPO ATUAL: %d\n" manager.time;

  print_running manager.running;
  print_process_list "PCB TABLE" manager.pcb_table;
  print_process_list "PROCESSOS PRONTOS" manager.ready_queue;
  print_process_list "PROCESSOS BLOQUEADOS" manager.blocked_queue;
  print_process_list "PROCESSOS TERMINADOS" manager.terminated;

  Printf.printf "==============================\n"
