open Process

type algorithm =
  | FCFS
  | Priority
  | SJFS
  | RM
  | EDF

let string_of_algorithm algorithm =
  match algorithm with
  | FCFS -> "FCFS"
  | Priority -> "Priority"
  | SJFS -> "SJFS"
  | RM -> "Rate Monotonic"
  | EDF -> "Earliest Deadline First"

let sort_queue algorithm ready_queue =
  match algorithm with
  | FCFS ->
      ready_queue

  | Priority ->
      List.sort
        (fun p1 p2 ->
          let cmp = compare p1.priority p2.priority in
          if cmp = 0 then compare p1.arrival_time p2.arrival_time
          else cmp)
        ready_queue

  | SJFS ->
      List.sort
        (fun p1 p2 ->
          let remaining1 = p1.program_size - p1.cpu_time in
          let remaining2 = p2.program_size - p2.cpu_time in
          let cmp = compare remaining1 remaining2 in
          if cmp = 0 then compare p1.arrival_time p2.arrival_time
          else cmp)
        ready_queue

  | RM ->
      List.sort
        (fun p1 p2 ->
          let cmp = compare p1.period p2.period in
          if cmp = 0 then compare p1.arrival_time p2.arrival_time
          else cmp)
        ready_queue

  | EDF ->
      List.sort
        (fun p1 p2 ->
          let cmp = compare p1.deadline p2.deadline in
          if cmp = 0 then compare p1.arrival_time p2.arrival_time
          else cmp)
        ready_queue

let schedule algorithm ready_queue =
  match sort_queue algorithm ready_queue with
  | [] -> None
  | process :: rest -> Some (process, rest)
