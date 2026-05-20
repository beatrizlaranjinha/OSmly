open Process

type algorithm =
  | FCFS
  | Priority
  | SJFS

let schedule_fcfs ready_queue =
  match ready_queue with
  | [] -> None
  | process :: rest -> Some (process, rest)

let schedule_priority ready_queue =
  match ready_queue with
  | [] -> None
  | first :: rest ->
      let selected =
        List.fold_left
          (fun best process ->
            if process.priority > best.priority then process else best)
          first
          rest
      in

      let remaining =
        List.filter
          (fun process -> process.pid <> selected.pid)
          ready_queue
      in

      Some (selected, remaining)

let schedule_sjfs ready_queue =
  match ready_queue with
  | [] -> None
  | first :: rest ->
      let selected =
        List.fold_left
          (fun best process ->
            let best_remaining =
              best.program_size - best.cpu_time
            in
            let process_remaining =
              process.program_size - process.cpu_time
            in
            if process_remaining < best_remaining then process else best)
          first
          rest
      in

      let remaining =
        List.filter
          (fun process -> process.pid <> selected.pid)
          ready_queue
      in

      Some (selected, remaining)

let schedule algorithm ready_queue =
  match algorithm with
  | FCFS -> schedule_fcfs ready_queue
  | Priority -> schedule_priority ready_queue
  | SJFS -> schedule_sjfs ready_queue
