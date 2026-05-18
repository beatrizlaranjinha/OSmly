(* Define o estado global do simulador. *)

(* Tipos de políticas de escalonamento suportadas pelo CPU. *)
type scheduling_policy =
  | FCFS
  | Priority
  | SJF
  | EDF

type manager = {
  time : int;                         (* Tempo atual da simulação *)
  memory : Memory.memory;             (* Estrutura de dados que representa a memória principal *)
  pcb_table : Process.pcb list;       (* Tabela com todos os processos criados *)
  new_queue : Process.pcb list;       (* Fila de processos a aguardar tempo de chegada *)
  ready_queue : Process.pcb list;     (* Fila de processos prontos a executar *)
  blocked_queue : Process.pcb list;   (* Fila de processos bloqueados *)
  terminated : Process.pcb list;      (* Lista de processos terminados *)
  running : Process.pcb option;       (* Processo atualmente em execução *)
  next_pid : int;                     (* Próximo identificador de processo (PID) disponível *)
  policy : scheduling_policy;         (* Política de escalonamento short-term ativa *)
}

(* Inicializa o estado do simulador com valores por omissão. *)
let create_manager memory policy =
  {
    time = 0;
    memory;
    pcb_table = [];
    new_queue = [];
    ready_queue = [];
    blocked_queue = [];
    terminated = [];
    running = None;
    next_pid = 1;
    policy;
  }

(* Cria um processo a partir de uma entrada do plano e adiciona-o à fila de prontos com alocação dinâmica. *)
let add_process_from_plan manager plan_entry =
  let program_opt = try Some (Memory.load_program_from_file ("data/" ^ plan_entry.Plan.program_name)) with Sys_error _ -> None in
  match program_opt with
  | None ->
      print_endline ("Erro: Programa " ^ plan_entry.Plan.program_name ^ " não encontrado. Processo não criado.");
      manager
  | Some program ->
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
        new_queue = manager.new_queue @ [pcb];
        next_pid = manager.next_pid + 1;
      }

(* Comando D: Escalonador de Longo Prazo. Desbloqueia processos da fila de bloqueados com uma probabilidade. *)
let command_D manager =
  match manager.blocked_queue with
  | [] ->
      print_endline "Nenhum processo para desbloquear.";
      manager
  | _ ->
      (* Probabilidade de 50% de desbloquear cada processo *)
      let to_wake, to_keep = List.partition (fun _ -> Random.bool ()) manager.blocked_queue in
      match to_wake with
      | [] ->
          print_endline "Nenhum processo teve a sorte de ser desbloqueado nesta ronda probabilística.";
          manager
      | _ ->
          let updated_woken = List.map (fun p -> Process.update_state p Process.Ready) to_wake in
          List.iter (fun p -> print_endline ("Processo " ^ string_of_int p.Process.pid ^ " desbloqueado probabilisticamente.")) updated_woken;
          { manager with
            blocked_queue = to_keep;
            ready_queue = manager.ready_queue @ updated_woken }

(* Comando I: Interrompe o próximo processo da fila e move-o para a fila de bloqueados. *)
let command_I manager =
  (* Como o processo running é None entre comandos, interrompemos quem seria o próximo a executar *)
  let sorted_queue = match manager.policy with
    | FCFS -> List.stable_sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) manager.ready_queue
    | Priority -> List.stable_sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) manager.ready_queue
    | SJF -> List.stable_sort (fun p1 p2 -> compare p1.Process.size p2.Process.size) manager.ready_queue
    | EDF ->
        List.stable_sort (fun p1 p2 -> 
          let d1 = p1.Process.arrival_time + p1.Process.priority in
          let d2 = p2.Process.arrival_time + p2.Process.priority in
          compare d1 d2) manager.ready_queue
  in
  match sorted_queue with
  | [] ->
      print_endline "Nenhum processo pronto para interceptar e interromper.";
      manager
  | pcb :: tail ->
      let updated_pcb = Process.update_state pcb Process.Blocked in
      print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " (" ^ pcb.Process.program_name ^ ") interceptado e bloqueado antes de executar.");
      { manager with
        ready_queue = tail;
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

  print_endline "PROCESSOS NOVOS (A aguardar chegada):";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name ^ " | Chega em: " ^ string_of_int p.Process.arrival_time)) manager.new_queue;

  print_endline "PROCESSOS PRONTOS:";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name)) manager.ready_queue;

  print_endline "PROCESSOS TERMINADOS:";
  List.iter (fun p -> print_endline ("  PID: " ^ string_of_int p.Process.pid ^ " | Programa: " ^ p.Process.program_name)) manager.terminated;
  print_endline "--------------------------------";
  manager

(* Limite de tempo de CPU atribuído a um processo (Time Quantum). *)
let quantum = 3

(* Promove processos da new_queue para a ready_queue se já tiver chegado a hora *)
let check_arrivals manager =
  let arrived, waiting = List.partition (fun p -> p.Process.arrival_time <= manager.time) manager.new_queue in
  let updated_arrived = List.map (fun p -> Process.update_state p Process.Ready) arrived in
  if List.length updated_arrived > 0 then
    List.iter (fun p -> print_endline ("Processo " ^ string_of_int p.Process.pid ^ " (" ^ p.Process.program_name ^ ") chegou ao sistema (T=" ^ string_of_int manager.time ^ ").")) updated_arrived;
  { manager with new_queue = waiting; ready_queue = manager.ready_queue @ updated_arrived }

(* Realiza o escalonamento curto e aplica o custo de comutação de contexto. *)
let schedule_and_switch manager =
  let manager_with_arrivals = check_arrivals manager in
  match manager_with_arrivals.running with
  | Some _ -> manager_with_arrivals
  | None ->
      match manager_with_arrivals.ready_queue with
      | [] -> manager_with_arrivals
      | _ ->
          (* Escalonador de Curto Prazo (Short-Term Scheduler) *)
          let sorted_queue = match manager_with_arrivals.policy with
            | FCFS -> 
                List.stable_sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) manager_with_arrivals.ready_queue
            | Priority -> 
                List.stable_sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) manager_with_arrivals.ready_queue
            | SJF -> 
                List.stable_sort (fun p1 p2 -> compare p1.Process.size p2.Process.size) manager_with_arrivals.ready_queue
            | EDF ->
                List.stable_sort (fun p1 p2 -> 
                  let d1 = p1.Process.arrival_time + p1.Process.priority in
                  let d2 = p2.Process.arrival_time + p2.Process.priority in
                  compare d1 d2) manager_with_arrivals.ready_queue
          in
          let pcb = List.hd sorted_queue in
          let tail = List.tl sorted_queue in
          let updated_pcb = Process.update_state pcb Process.Running in
          print_endline ("Context Switch: Processo " ^ string_of_int pcb.Process.pid ^ " escalonado (+1 tempo). Política ativa: " ^ 
            match manager_with_arrivals.policy with FCFS -> "FCFS" | Priority -> "Priority" | SJF -> "SJF" | EDF -> "EDF");
          { manager_with_arrivals with running = Some updated_pcb; ready_queue = tail; time = manager_with_arrivals.time + 1 }

(* Ciclo de Fetch-Decode-Execute. *)
let rec execute_quantum manager pcb quantum_left =
  if quantum_left = 0 then
    let updated_pcb = Process.update_state pcb Process.Ready in
    { manager with running = None; ready_queue = manager.ready_queue @ [updated_pcb] }
  else if pcb.Process.pc >= pcb.Process.start_address + pcb.Process.size || pcb.Process.pc < pcb.Process.start_address then
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
             (* Duplica o valor da variável do processo pai para o processo filho. *)
             let child_pcb = Process.update_value child_pcb pcb.Process.value in
             (* Atualiza o PC do processo pai, saltando n instruções. *)
             let parent_pcb = Process.update_pc pcb (pcb.Process.pc + n + 1) in
             let manager_with_child = { manager_advanced with memory = new_mem; next_pid = manager_advanced.next_pid + 1; ready_queue = manager_advanced.ready_queue @ [child_pcb]; pcb_table = child_pcb :: manager_advanced.pcb_table } in
             execute_quantum manager_with_child parent_pcb (quantum_left - 1))
    | Instructions.L filename ->
        let pcb = Process.increment_cpu_time pcb in
        let temp_mem = Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size in
        (* Previne falhas (Sys_error) ao tentar carregar um ficheiro inexistente. *)
        let program_opt = try Some (Memory.load_program_from_file ("data/" ^ filename)) with Sys_error _ -> None in
        (match program_opt with
         | None ->
             print_endline ("Erro: Ficheiro " ^ filename ^ " não encontrado (L). Processo terminado.");
             let term_pcb = Process.terminate_process pcb manager_advanced.time in
             { manager_advanced with memory = temp_mem; running = None; terminated = manager_advanced.terminated @ [term_pcb] }
         | Some program ->
             let size = List.length program in
             (match Memory.allocate_memory temp_mem size with
              | None ->
                  print_endline ("Erro: Sem memória para carregar " ^ filename ^ " (L). Processo terminado.");
                  let term_pcb = Process.terminate_process pcb manager_advanced.time in
                  { manager_advanced with memory = temp_mem; running = None; terminated = manager_advanced.terminated @ [term_pcb] }
              | Some start_address ->
                  (* Liberta a memória antiga e carrega o novo programa, repondo a variável a zero. *)
                  let new_mem = Memory.load_program temp_mem program start_address in
                  let exec_pcb = { pcb with Process.program_name = filename; start_address = start_address; size = size; pc = start_address; value = 0 } in
                  let manager_with_prog = { manager_advanced with memory = new_mem } in
                  execute_quantum manager_with_prog exec_pcb (quantum_left - 1)))
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
