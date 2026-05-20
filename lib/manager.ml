open Process
open Memory
open Plan
open Scheduler
open Dispatcher

type manager = {
  memory : memory;
  mutable time : int;
  mutable next_pid : int;
  mutable plan : plan_entry list;
  mutable pcb_table : pcb list;
  mutable ready_queue : pcb list;
  mutable blocked_queue : pcb list;
  mutable terminated : pcb list;
  mutable running : pcb option;
  algorithm : algorithm;
  quantum : int;
}

let create_manager plan_file algorithm quantum =
  {
    memory = create_memory ();
    time = 0;
    next_pid = 1;
    plan = load_plan plan_file;
    pcb_table = [];
    ready_queue = [];
    blocked_queue = [];
    terminated = [];
    running = None;
    algorithm;
    quantum;
  }

let create_process manager entry =
  let path = "data/" ^ entry.program_name in
  let program = load_program_from_file path in
  let start = load_program manager.memory program in
  let size = List.length program in

  let process =
    create_pcb
      manager.next_pid
      0
      entry.program_name
      start
      size
      entry.priority
      entry.arrival_time
  in

  manager.next_pid <- manager.next_pid + 1;
  manager.pcb_table <- manager.pcb_table @ [process];
  process

let add_arrivals manager =
  let arrivals = arrivals_at_time manager.time manager.plan in

  let new_processes =
    List.map
      (fun entry -> create_process manager entry)
      arrivals
  in

  manager.ready_queue <- manager.ready_queue @ new_processes;
  manager.plan <- remove_arrivals_at_time manager.time manager.plan

let dispatch_next manager =
  match schedule manager.algorithm manager.ready_queue with
  | None ->
      manager.running <- None

  | Some (process, rest) ->
      process.state <- Running;
      manager.ready_queue <- rest;
      manager.running <- Some process

let handle_exec manager process filename =
  let file =
    if Filename.check_suffix filename ".prg"
    then filename
    else filename ^ ".prg"
  in

  let path = "data/" ^ file in
  let program = load_program_from_file path in
  let start = load_program manager.memory program in
  let size = List.length program in

  let updated_process =
    {
      process with
      program_name = file;
      start_address = start;
      program_size = size;
      pc = start;
      state = Ready;
    }
  in

  manager.ready_queue <- manager.ready_queue @ [updated_process];
  manager.pcb_table <-
    List.map
      (fun p ->
        if p.pid = updated_process.pid then updated_process else p)
      manager.pcb_table

let handle_dispatch_result manager process result =
  match result with
  | Continue ->
      ()

  | Blocked ->
      manager.blocked_queue <- manager.blocked_queue @ [process]

  | Terminated ->
      manager.terminated <- manager.terminated @ [process]

  | Fork child ->
      manager.next_pid <- manager.next_pid + 1;
      child.state <- Ready;
      process.state <- Ready;

      manager.pcb_table <- manager.pcb_table @ [child];
      manager.ready_queue <- manager.ready_queue @ [child; process]

  | Exec filename ->
      handle_exec manager process filename

let execute_one_instruction manager process =
  let result =
    dispatch_instruction
      manager.memory
      process
      manager.next_pid
      manager.time
  in

  manager.time <- manager.time + 1;
  add_arrivals manager;
  handle_dispatch_result manager process result;
  result

let rec execute_quantum manager process remaining =
  if remaining = 0 then (
    process.state <- Ready;
    manager.ready_queue <- manager.ready_queue @ [process]
  )
  else
    let result = execute_one_instruction manager process in
    match result with
    | Continue ->
        execute_quantum manager process (remaining - 1)

    | Blocked
    | Terminated
    | Fork _
    | Exec _ ->
        ()

let execute manager =
  add_arrivals manager;

  match manager.running with
  | Some _ ->
      ()

  | None ->
      dispatch_next manager;

      match manager.running with
      | None ->
          manager.time <- manager.time + 1

      | Some process ->
          execute_quantum manager process manager.quantum;
          manager.running <- None

let unblock_one manager =
  match manager.blocked_queue with
  | [] ->
      ()

  | process :: rest ->
      process.state <- Ready;
      manager.blocked_queue <- rest;
      manager.ready_queue <- manager.ready_queue @ [process]
