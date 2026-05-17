(* Define o estado global do simulador. *)

type manager = {
  time : int;                         (* Tempo atual da simulação *)
  memory : Memory.memory;             (* Estrutura de dados que representa a memória principal *)
  pcb_table : Process.pcb list;       (* Tabela com todos os processos criados *)
  ready_queue : Process.pcb list;     (* Fila de processos prontos a executar *)
  blocked_queue : Process.pcb list;   (* Fila de processos bloqueados *)
  terminated : Process.pcb list;      (* Lista de processos terminados *)
  running : Process.pcb option;       (* Processo atualmente em execução *)
  next_pid : int;                     (* Próximo identificador de processo (PID) disponível *)
}

(* Inicializa o estado do simulador com valores por omissão. *)
let create_manager memory =
  {
    time = 0;
    memory;
    pcb_table = [];
    ready_queue = [];
    blocked_queue = [];
    terminated = [];
    running = None;
    next_pid = 1;
  }

(* Cria um processo a partir de uma entrada do plano e adiciona-o à fila de prontos com alocação dinâmica. *)
let add_process_from_plan manager plan_entry =
  let program = Memory.load_program_from_file ("data/" ^ plan_entry.Plan.program_name) in
  let size = List.length program in

  match Memory.allocate_memory manager.memory size with
  | None ->
      print_endline ("Erro: Memória insuficiente para carregar " ^ plan_entry.Plan.program_name);
      manager
  | Some start_address ->
      let memory = Memory.load_program manager.memory program start_address in
      let pcb = Process.create_pcb manager.next_pid 0 plan_entry.Plan.program_name start_address size plan_entry.Plan.priority plan_entry.Plan.arrival_time in
      {
        manager with
        memory = memory;
        pcb_table = pcb :: manager.pcb_table;
        ready_queue = manager.ready_queue @ [pcb];
        next_pid = manager.next_pid + 1;
      }

(* Comando D: Desbloqueia um processo aleatório da fila de bloqueados e move-o para a fila de prontos. *)
let command_D manager =
  match manager.blocked_queue with
  | [] ->
      print_endline "Nenhum processo para desbloquear.";
      manager
  | _ ->
      let len = List.length manager.blocked_queue in
      let idx = Random.int len in
      let pcb = List.nth manager.blocked_queue idx in
      let before = List.filteri (fun i _ -> i < idx) manager.blocked_queue in
      let after = List.filteri (fun i _ -> i > idx) manager.blocked_queue in
      let tail = before @ after in
      let updated_pcb = Process.update_state pcb Process.Ready in
      print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " desbloqueado.");
      { manager with
        blocked_queue = tail;
        ready_queue = manager.ready_queue @ [updated_pcb] }

(* Comando I: Interrompe o processo em execução e move-o para a fila de bloqueados. *)
let command_I manager =
  match manager.running with
  | None ->
      print_endline "Nenhum processo em execução para interromper.";
      manager
  | Some pcb ->
      let updated_pcb = Process.update_state pcb Process.Blocked in
      print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " interrompido e bloqueado.");
      { manager with
        running = None;
        blocked_queue = manager.blocked_queue @ [updated_pcb] }

(* Comando R: Imprime o estado atual do simulador. *)
let command_R manager =
  print_endline ("\n--- REPORT [Tempo Atual: " ^ string_of_int manager.time ^ "] ---");
  (match manager.running with
   | None -> print_endline "PROCESSO EM EXECUÇÃO: Nenhum"
   | Some p -> 
       print_endline ("PROCESSO EM EXECUÇÃO:\n  PID: " ^ string_of_int p.Process.pid ^ 
                      " | Programa: " ^ p.Process.program_name ^ 
                      " | PC: " ^ string_of_int p.Process.pc ^
                      " | Valor: " ^ string_of_int p.Process.value));

  print_endline "PROCESSOS BLOQUEADOS:";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name)) manager.blocked_queue;

  print_endline "PROCESSOS PRONTOS:";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name)) manager.ready_queue;

  print_endline "PROCESSOS TERMINADOS:";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name)) manager.terminated;
  print_endline "--------------------------------";
  manager

(* Limite de tempo de CPU atribuído a um processo (Time Quantum). *)
let quantum = 3

(* Realiza o escalonamento curto (Priority) e aplica o custo de comutação de contexto. *)
let schedule_and_switch manager =
  match manager.running with
  | Some _ -> manager
  | None ->
      match manager.ready_queue with
      | [] -> manager
      | _ ->
          (* Escalonamento por Prioridades (Short-Term Scheduler) *)
          let sorted_queue = List.stable_sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) manager.ready_queue in
          let pcb = List.hd sorted_queue in
          let tail = List.tl sorted_queue in
          let updated_pcb = Process.update_state pcb Process.Running in
          print_endline ("Context Switch: Processo " ^ string_of_int pcb.Process.pid ^ " escalonado (+1 tempo).");
          { manager with running = Some updated_pcb; ready_queue = tail; time = manager.time + 1 }

(* Ciclo de Fetch-Decode-Execute. *)
let rec execute_quantum manager pcb quantum_left =
  if quantum_left = 0 then
    let updated_pcb = Process.update_state pcb Process.Ready in
    { manager with running = None; ready_queue = manager.ready_queue @ [updated_pcb] }
  else if pcb.Process.pc >= pcb.Process.start_address + pcb.Process.size then
    let term_pcb = Process.terminate_process pcb manager.time in
    let new_mem = Memory.free_memory manager.memory pcb.Process.start_address pcb.Process.size in
    { manager with memory = new_mem; running = None; terminated = manager.terminated @ [term_pcb] }
  else
    let instr = manager.memory.(pcb.Process.pc) in
    let manager_advanced = { manager with time = manager.time + 1 } in
    match instr with
    | Instructions.M n ->
        let pcb = Process.update_value pcb n in
        let pcb = Process.increment_pc pcb in
        let pcb = Process.increment_cpu_time pcb in
        execute_quantum manager_advanced pcb (quantum_left - 1)
    | Instructions.A n ->
        let pcb = Process.update_value pcb (pcb.Process.value + n) in
        let pcb = Process.increment_pc pcb in
        let pcb = Process.increment_cpu_time pcb in
        execute_quantum manager_advanced pcb (quantum_left - 1)
    | Instructions.S n ->
        let pcb = Process.update_value pcb (pcb.Process.value - n) in
        let pcb = Process.increment_pc pcb in
        let pcb = Process.increment_cpu_time pcb in
        execute_quantum manager_advanced pcb (quantum_left - 1)
    | Instructions.B ->
        let pcb = Process.increment_pc pcb in
        let pcb = Process.increment_cpu_time pcb in
        let pcb = Process.update_state pcb Process.Blocked in
        print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " bloqueado (Instrução B).");
        { manager_advanced with running = None; blocked_queue = manager_advanced.blocked_queue @ [pcb] }
    | Instructions.T ->
        let pcb = Process.increment_cpu_time pcb in
        let pcb = Process.update_state pcb Process.Terminated in
        let term_pcb = { pcb with Process.end_time = Some manager_advanced.time } in
        print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " terminou (Instrução T).");
        let new_mem = Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size in
        { manager_advanced with memory = new_mem; running = None; terminated = manager_advanced.terminated @ [term_pcb] }
    | Instructions.C n ->
        let pcb = Process.increment_cpu_time pcb in
        (match Memory.allocate_memory manager_advanced.memory pcb.Process.size with
         | None ->
             print_endline "Aviso: Sem memória para processo filho (C). Instrução ignorada.";
             let pcb = Process.increment_pc pcb in
             execute_quantum manager_advanced pcb (quantum_left - 1)
         | Some child_start ->
             let new_mem = Array.copy manager_advanced.memory in
             Array.blit manager_advanced.memory pcb.Process.start_address new_mem child_start pcb.Process.size;
             let child_pcb = Process.create_pcb manager_advanced.next_pid pcb.Process.pid pcb.Process.program_name child_start pcb.Process.size pcb.Process.priority manager_advanced.time in
             let child_pcb = Process.update_pc child_pcb (child_start + (pcb.Process.pc - pcb.Process.start_address) + 1) in
             let parent_pcb = Process.update_pc pcb (pcb.Process.pc + n + 1) in
             let manager_with_child = { manager_advanced with memory = new_mem; next_pid = manager_advanced.next_pid + 1; ready_queue = manager_advanced.ready_queue @ [child_pcb]; pcb_table = child_pcb :: manager_advanced.pcb_table } in
             execute_quantum manager_with_child parent_pcb (quantum_left - 1))
    | Instructions.L filename ->
        let pcb = Process.increment_cpu_time pcb in
        let program = Memory.load_program_from_file ("data/" ^ filename) in
        let size = List.length program in
        (match Memory.allocate_memory manager_advanced.memory size with
         | None ->
             print_endline ("Aviso: Sem memória para carregar " ^ filename ^ " (L). Instrução ignorada.");
             let pcb = Process.increment_pc pcb in
             execute_quantum manager_advanced pcb (quantum_left - 1)
         | Some start_address ->
             let temp_mem = Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size in
             let new_mem = Memory.load_program temp_mem program start_address in
             let exec_pcb = { pcb with Process.program_name = filename; start_address = start_address; size = size; pc = start_address } in
             let manager_with_prog = { manager_advanced with memory = new_mem } in
             execute_quantum manager_with_prog exec_pcb (quantum_left - 1))
    | Instructions.Empty ->
        let pcb = Process.increment_pc pcb in
        let pcb = Process.increment_cpu_time pcb in
        execute_quantum manager_advanced pcb (quantum_left - 1)

(* Comando E: Executa um processo durante o time quantum definido. *)
let command_E manager =
  let manager_scheduled = schedule_and_switch manager in
  match manager_scheduled.running with
  | None ->
      print_endline "Nenhum processo pronto para executar.";
      { manager_scheduled with time = manager_scheduled.time + 1 }
  | Some pcb ->
      execute_quantum manager_scheduled pcb quantum
