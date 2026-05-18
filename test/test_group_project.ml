open Group_project

let test_command_E () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 10 1 0 in
  let manager_with_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after_E = Manager.command_E manager_with_ready in
  
  assert (manager_after_E.Manager.time = 4);
  match manager_after_E.Manager.ready_queue with
  | [p] ->
      assert (p.Process.pid = 1);
      assert (p.Process.cpu_time = 3);
      assert (p.Process.state = Process.Ready)
  | _ -> failwith "Erro no teste command_E"

let test_command_I () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 1 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  let manager_after_I = Manager.command_I manager_ready in
  
  match manager_after_I.Manager.blocked_queue with
  | [p] ->
      assert (p.Process.pid = 1);
      assert (p.Process.state = Process.Blocked)
  | _ -> failwith "Erro no teste command_I"

let test_command_D () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "p1" 0 1 1 0 in
  let pcb2 = Process.create_pcb 2 0 "p2" 0 1 1 0 in
  let p1_blocked = Process.update_state pcb1 Process.Blocked in
  let p2_blocked = Process.update_state pcb2 Process.Blocked in
  
  let manager_blocked = { manager with Manager.blocked_queue = [p1_blocked; p2_blocked] } in
  let manager_after_D = Manager.command_D manager_blocked in
  
  (* Verifica se o total de processos bloqueados e prontos se mantém (2) *)
  let total_procs = List.length manager_after_D.Manager.blocked_queue + List.length manager_after_D.Manager.ready_queue in
  assert (total_procs = 2)

let test_priority_scheduler () =
  let memory = Memory.create_memory () in
  let manager = Manager.create_manager memory Manager.Priority in
  
  (* Processos com diferentes prioridades *)
  let pcb_low = Process.create_pcb 1 0 "low_prio" 0 1 5 0 in
  let pcb_high = Process.create_pcb 2 0 "high_prio" 0 1 1 0 in
  
  (* pcb_low está primeiro na fila, mas pcb_high tem melhor prioridade (1 < 5) *)
  let manager_ready = { manager with Manager.ready_queue = [pcb_low; pcb_high] } in
  let manager_after_sched = Manager.schedule_and_switch manager_ready in
  
  match manager_after_sched.Manager.running with
  | Some p -> assert (p.Process.pid = 2) (* Garante que o High Priority assumiu o CPU *)
  | None -> failwith "Nenhum processo escalonado no priority test"

let test_execution_math () =
  let memory = Memory.create_memory () in
  
  (* Injetar instruções de teste na memória *)
  memory.(0) <- Instructions.M 10; (* Meter valor 10 *)
  memory.(1) <- Instructions.A 5;  (* Adicionar 5 = 15 *)
  memory.(2) <- Instructions.S 3;  (* Subtrair 3 = 12 *)
  
  let manager = Manager.create_manager memory Manager.Priority in
  let pcb1 = Process.create_pcb 1 0 "math_prog" 0 3 1 0 in
  let manager_ready = { manager with Manager.ready_queue = [pcb1] } in
  
  (* O Quantum é 3, por isso vai correr as três instruções de rajada *)
  let manager_after_E = Manager.command_E manager_ready in
  
  match manager_after_E.Manager.ready_queue with
  | [p] -> 
      (* Verifica se o resultado das contas matemáticas está lá *)
      assert (p.Process.value = 12);
      assert (p.Process.pc = 3)
  | _ -> failwith "Erro no execution math: Processo não voltou à ready_queue"

let test_memory_allocation () =
  let memory = Memory.create_memory () in
  
  (* Tentar alocar 100 posições num array vazio -> deve ficar no 0 *)
  let addr1_opt = Memory.allocate_memory memory 100 in
  assert (addr1_opt = Some 0);
  
  (* Falsificar alocação inserindo 'T' para preencher a memória *)
  for i = 0 to 99 do memory.(i) <- Instructions.T done;
  
  (* Tentar alocar 50 posições agora -> deve ficar no 100 *)
  let addr2_opt = Memory.allocate_memory memory 50 in
  assert (addr2_opt = Some 100);
  
  (* Limpar os primeiros 100 *)
  let new_memory = Memory.free_memory memory 0 100 in
  
  (* Tentar alocar 20 -> com o novo First Fit, deve voltar ao índice 0 (pois libertámos) *)
  let addr3_opt = Memory.allocate_memory new_memory 20 in
  assert (addr3_opt = Some 0)


let () =
  print_endline "A executar Suite de Testes Base...";
  test_command_E ();
  test_command_I ();
  test_command_D ();
  print_endline "A executar Suite de Testes Funcionais (Novas Otimizações)...";
  test_priority_scheduler ();
  test_execution_math ();
  test_memory_allocation ();
  print_endline "-> Todos os 6 Testes Unitários passaram com sucesso!"
