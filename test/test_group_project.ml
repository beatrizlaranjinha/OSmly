open Group_project

let test_memory_allocation () =
  let memory = Memory.create_memory () in
  let addr1_opt = Memory.allocate_memory memory 100 in
  assert (addr1_opt = Some 0);
  for i = 0 to 99 do memory.(i) <- Instructions.T done;
  let addr2_opt = Memory.allocate_memory memory 50 in
  assert (addr2_opt = Some 100);
  let new_memory = Memory.free_memory memory 0 100 in
  let addr3_opt = Memory.allocate_memory new_memory 20 in
  assert (addr3_opt = Some 0)

let test_command_E_math () =
  let memory = Memory.create_memory () in
  memory.(0) <- Instructions.M 10;
  memory.(1) <- Instructions.A 5;
  memory.(2) <- Instructions.S 3;
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "prog" 0 3 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after = Manager.command_E manager_ready in
  match manager_after.Manager.ready_queue with
  | [p] -> 
      assert (p.Process.value = 12);
      assert (p.Process.pc = 3)
  | _ -> failwith "Erro no test_command_E_math"

let test_command_E_block () =
  let memory = Memory.create_memory () in
  memory.(0) <- Instructions.B;
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "prog" 0 1 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after = Manager.command_E manager_ready in
  match manager_after.Manager.blocked_queue with
  | [p] -> 
      assert (p.Process.state = Process.Blocked);
      assert (p.Process.pc = 1)
  | _ -> failwith "Erro no test_command_E_block"

let test_command_E_terminate () =
  let memory = Memory.create_memory () in
  memory.(0) <- Instructions.T;
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "prog" 0 1 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after = Manager.command_E manager_ready in
  match manager_after.Manager.terminated with
  | [p] -> 
      assert (p.Process.state = Process.Terminated);
      assert (p.Process.end_time <> None)
  | _ -> failwith "Erro no test_command_E_terminate"

let test_command_E_fork () =
  let memory = Memory.create_memory () in
  memory.(0) <- Instructions.C 2;
  memory.(1) <- Instructions.A 1;
  memory.(2) <- Instructions.A 1;
  memory.(3) <- Instructions.A 1;
  memory.(4) <- Instructions.A 1;
  memory.(5) <- Instructions.A 1;
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "prog" 0 6 1 0 in
  let pcb1 = Process.update_value pcb1 10 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1]; Manager.next_pid = 2 } in
  let manager_after = Manager.command_E manager_ready in
  assert (List.length manager_after.Manager.ready_queue = 2);
  let p_pai = List.find (fun p -> p.Process.pid = 1) manager_after.Manager.ready_queue in
  let p_filho = List.find (fun p -> p.Process.ppid = 1) manager_after.Manager.ready_queue in
  assert (p_pai.Process.pc = 5);
  assert (p_filho.Process.value = 10)

let test_command_I () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 1 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after = Manager.command_I manager_ready in
  match manager_after.Manager.blocked_queue with
  | [p] -> assert (p.Process.state = Process.Blocked)
  | _ -> failwith "Erro no test_command_I"

let test_command_D () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 1 1 0 in
  let p1_blocked = Process.update_state pcb1 Process.Blocked in
  let manager_blocked = { manager with Manager.blocked_queue = [p1_blocked] } in
  let manager_after = Manager.command_D manager_blocked in
  let total = List.length manager_after.Manager.blocked_queue + List.length manager_after.Manager.ready_queue in
  assert (total = 1)

let test_command_R () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let manager_after = Manager.command_R manager in
  assert (manager.Manager.time = manager_after.Manager.time)

let test_scheduler_priority () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb_low = Process.create_pcb 1 0 "p1" 0 1 5 0 in
  let pcb_high = Process.create_pcb 2 0 "p2" 0 1 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb_low; pcb_high] } in
  let manager_after = Manager.schedule_and_switch manager_ready in
  match manager_after.Manager.running with
  | Some p -> assert (p.Process.pid = 2)
  | None -> failwith "Erro no test_scheduler_priority"

let test_scheduler_fcfs () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.FCFS in
  let pcb2 = Process.create_pcb 2 0 "p2" 0 1 1 10 in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 1 1 5 in
  let manager_ready = { manager with Manager.ready_queue = [pcb2; pcb1] } in
  let manager_after = Manager.schedule_and_switch manager_ready in
  match manager_after.Manager.running with
  | Some p -> assert (p.Process.pid = 1)
  | None -> failwith "Erro no test_scheduler_fcfs"

let test_scheduler_sjf () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.SJF in
  let pcb_large = Process.create_pcb 1 0 "p1" 0 50 1 0 in
  let pcb_small = Process.create_pcb 2 0 "p2" 0 10 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb_large; pcb_small] } in
  let manager_after = Manager.schedule_and_switch manager_ready in
  match manager_after.Manager.running with
  | Some p -> assert (p.Process.pid = 2)
  | None -> failwith "Erro no test_scheduler_sjf"

let test_scheduler_edf () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.EDF in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 10 10 0 in
  let pcb2 = Process.create_pcb 2 0 "p2" 0 10 2 5 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1; pcb2] } in
  let manager_after = Manager.schedule_and_switch manager_ready in
  match manager_after.Manager.running with
  | Some p -> assert (p.Process.pid = 2)
  | None -> failwith "Erro no test_scheduler_edf"

let test_arrivals () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 10 1 5 in
  let manager_new = { manager with Manager.new_queue = [pcb1]; Manager.time = 5 } in
  let manager_after = Manager.check_arrivals manager_new in
  assert (List.length manager_after.Manager.ready_queue = 1);
  assert (List.length manager_after.Manager.new_queue = 0)

let () =
  print_endline "A iniciar suite de testes globais do OSmly...";
  test_memory_allocation ();
  test_command_E_math ();
  test_command_E_block ();
  test_command_E_terminate ();
  test_command_E_fork ();
  test_command_I ();
  test_command_D ();
  test_command_R ();
  test_scheduler_priority ();
  test_scheduler_fcfs ();
  test_scheduler_sjf ();
  test_scheduler_edf ();
  test_arrivals ();
  print_endline "Todos os testes unitários completados sem erros!"
