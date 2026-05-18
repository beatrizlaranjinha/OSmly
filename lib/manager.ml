(* Define o estado global do simulador. *)

(* Tipos de políticas de escalonamento suportadas pelo CPU. *)
type scheduling_policy =
  | FCFS
  | Priority
  | SJF
  | EDF
  | RM

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

(* Retorna uma lista de blocos alocados na memória, baseados nos PCBs ativos *)
let get_allocated_blocks manager =
  let active_pcbs = manager.new_queue @ manager.ready_queue @ manager.blocked_queue @
    (match manager.running with Some p -> [p] | None -> []) in
  let blocks = List.map (fun p -> (p.Process.start_address, p.Process.size)) active_pcbs in
  List.sort_uniq (fun (a, _) (b, _) -> compare a b) blocks

(* Verifica se um bloco de memória está em uso por outros processos *)
let is_memory_in_use manager start_address exclude_pid =
  let active_pcbs = manager.new_queue @ manager.ready_queue @ manager.blocked_queue @
    (match manager.running with Some p -> [p] | None -> []) in
  List.exists (fun p -> p.Process.start_address = start_address && p.Process.pid <> exclude_pid) active_pcbs

(* Verifica se um programa já está carregado na memória. Se sim, devolve (start_address, size) *)
let find_loaded_program manager prog_name =
  let active_pcbs = manager.new_queue @ manager.ready_queue @ manager.blocked_queue @
    (match manager.running with Some p -> [p] | None -> []) in
  let found = List.find_opt (fun p -> p.Process.program_name = prog_name) active_pcbs in
  match found with
  | Some p -> Some (p.Process.start_address, p.Process.size)
  | None -> None

(* Desfragmenta a memória, juntando os blocos e atualizando os endereços de todos os processos *)
let defragment_memory manager =
  print_endline "Aviso: Memória fragmentada. A iniciar desfragmentação...";
  let unique_blocks = get_allocated_blocks manager in
  let new_memory = Memory.create_memory () in
  let rec move_blocks blocks current_free block_map =
    match blocks with
    | [] -> block_map
    | (old_start, size) :: rest ->
        Array.blit manager.memory old_start new_memory current_free size;
        move_blocks rest (current_free + size) ((old_start, current_free) :: block_map)
  in
  let block_map = move_blocks unique_blocks 0 [] in
  let update_pcb pcb =
    match List.assoc_opt pcb.Process.start_address block_map with
    | Some new_start -> 
        let offset = pcb.Process.pc - pcb.Process.start_address in
        { pcb with Process.start_address = new_start; Process.pc = new_start + offset }
    | None -> pcb
  in
  let new_new = List.map update_pcb manager.new_queue in
  let new_ready = List.map update_pcb manager.ready_queue in
  let new_blocked = List.map update_pcb manager.blocked_queue in
  let new_running = match manager.running with None -> None | Some p -> Some (update_pcb p) in
  { manager with
    memory = new_memory;
    new_queue = new_new;
    ready_queue = new_ready;
    blocked_queue = new_blocked;
    running = new_running;
  }

(* Tenta alocar memória. Se falhar, desfragmenta e tenta novamente *)
let allocate_with_defrag manager size =
  let blocks = get_allocated_blocks manager in
  match Memory.allocate_memory blocks size with
  | Some addr -> (manager, Some addr)
  | None ->
      let defragged = defragment_memory manager in
      let new_blocks = get_allocated_blocks defragged in
      match Memory.allocate_memory new_blocks size with
      | Some addr -> (defragged, Some addr)
      | None -> (defragged, None)

(* Cria um processo a partir de uma entrada do plano e adiciona-o à fila de prontos com alocação dinâmica. *)
let add_process_from_plan manager plan_entry =
  if String.contains plan_entry.Plan.program_name '/' || String.contains plan_entry.Plan.program_name '\\' || String.contains plan_entry.Plan.program_name '\000' then
    (print_endline ("Erro: Caminho inválido (Tentativa de Path Traversal/Injeção) para " ^ plan_entry.Plan.program_name); manager)
  else
    match find_loaded_program manager plan_entry.Plan.program_name with
    | Some (start_address, size) ->
        print_endline ("Partilha de Memória: Programa " ^ plan_entry.Plan.program_name ^ " reutilizado (start: " ^ string_of_int start_address ^ ")");
        let pcb = Process.create_pcb manager.next_pid 0 plan_entry.Plan.program_name start_address size plan_entry.Plan.priority plan_entry.Plan.arrival_time plan_entry.Plan.period in
        { manager with
          pcb_table = pcb :: manager.pcb_table;
          new_queue = manager.new_queue @ [pcb];
          next_pid = manager.next_pid + 1;
        }
    | None ->
        let program_opt = try Some (Memory.load_program_from_file ("data/" ^ plan_entry.Plan.program_name)) with Sys_error _ -> None in
        match program_opt with
        | None ->
            print_endline ("Erro: Programa " ^ plan_entry.Plan.program_name ^ " não encontrado. Processo não criado.");
            manager
        | Some (size, program) ->
            let manager, addr_opt = allocate_with_defrag manager size in
            match addr_opt with
            | None ->
                print_endline ("Erro: Memória insuficiente para carregar " ^ plan_entry.Plan.program_name);
                manager
            | Some start_address ->
                let memory = Memory.load_program manager.memory program start_address in
                let pcb = Process.create_pcb manager.next_pid 0 plan_entry.Plan.program_name start_address size plan_entry.Plan.priority plan_entry.Plan.arrival_time plan_entry.Plan.period in
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

(* Comando I: Interrompe o processo em execução e bloquei-o. *)
let command_I manager =
  match manager.running with
  | Some pcb ->
      let updated_pcb = Process.update_state pcb Process.Blocked in
      print_endline ("Processo " ^ string_of_int pcb.Process.pid ^ " (" ^ pcb.Process.program_name ^ ") interrompido e bloqueado!");
      { manager with running = None; blocked_queue = manager.blocked_queue @ [updated_pcb] }
  | None ->
      print_endline "Nenhum processo em execução para interromper.";
      manager

(* Formatador auxiliar para o PCB normal *)
let string_of_pcb p =
  Printf.sprintf "%d, %d, %d, %d, %d, %d"
    p.Process.pid p.Process.ppid p.Process.priority p.Process.value p.Process.arrival_time p.Process.cpu_time

(* Formatador auxiliar para PCB terminado *)
let string_of_term_pcb p =
  let end_t = match p.Process.end_time with Some t -> t | None -> 0 in
  Printf.sprintf "%d, %d, %d, %d, %d, %d, %d"
    p.Process.pid p.Process.ppid p.Process.priority p.Process.value p.Process.arrival_time end_t p.Process.cpu_time

(* Comando R: Imprime o estado atual do simulador. *)
let command_R manager =
  print_endline ("TEMPO ACTUAL: " ^ string_of_int manager.time);
  print_endline "PROCESSO EM EXECUÇÃO:";
  (match manager.running with
   | None -> ()
   | Some p -> print_endline (string_of_pcb p));

  print_endline "PROCESSOS BLOQUEADOS:\nFila dos processos";
  List.iter (fun p -> print_endline (string_of_pcb p)) manager.blocked_queue;

  print_endline "PROCESSOS PRONTOS A EXECUTAR\nFila dos processos";
  List.iter (fun p -> print_endline (string_of_pcb p)) manager.ready_queue;

  print_endline "PROCESSOS TERMINADOS";
  List.iter (fun p -> print_endline (string_of_term_pcb p)) manager.terminated;
  print_endline "--------------------------------";
  manager

(* Comando T: Imprime estatísticas globais e finaliza a simulação. *)
let command_T manager =
  let _ = command_R manager in
  print_endline "\n=== ESTATÍSTICAS GLOBAIS ===";
  let term_list = manager.terminated in
  let num_term = List.length term_list in
  if num_term = 0 then
    print_endline "Nenhum processo terminou. Sem estatísticas globais."
  else begin
    let total_turnaround = ref 0 in
    let total_waiting = ref 0 in
    List.iter (fun p ->
      let end_t = match p.Process.end_time with Some t -> t | None -> 0 in
      let turnaround = end_t - p.Process.arrival_time in
      let waiting = turnaround - p.Process.cpu_time in
      total_turnaround := !total_turnaround + turnaround;
      total_waiting := !total_waiting + waiting;
      Printf.printf "PID %d: Turnaround=%d, Waiting=%d\n" p.Process.pid turnaround waiting;
    ) term_list;
    Printf.printf "\nTempo Médio de Turnaround: %.2f\n" (float_of_int !total_turnaround /. float_of_int num_term);
    Printf.printf "Tempo Médio de Espera (Waiting): %.2f\n" (float_of_int !total_waiting /. float_of_int num_term);
  end;
  print_endline "============================";
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

(* Escolhe o próximo processo a executar com base na política ativa. *)
let schedule_and_switch manager =
  let manager_with_arrivals = check_arrivals manager in
  (* 1. Preempção: Se havia um processo a correr, volta para os Prontos *)
  let manager_preempted = match manager_with_arrivals.running with
    | Some p -> 
        let updated_p = Process.update_state p Process.Ready in
        { manager_with_arrivals with running = None; ready_queue = manager_with_arrivals.ready_queue @ [updated_p] }
    | None -> manager_with_arrivals
  in
  match manager_preempted.ready_queue with
  | [] -> manager_preempted
  | _ ->
      (* Escalonador de Curto Prazo (Short-Term Scheduler) *)
      let sorted_queue = match manager_preempted.policy with
        | FCFS -> 
            List.stable_sort (fun p1 p2 -> compare p1.Process.arrival_time p2.Process.arrival_time) manager_preempted.ready_queue
        | Priority -> 
            List.stable_sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) manager_preempted.ready_queue
        | SJF -> 
            List.stable_sort (fun p1 p2 -> compare p1.Process.size p2.Process.size) manager_preempted.ready_queue
        | RM ->
            List.stable_sort (fun p1 p2 ->
              let period1 = match p1.Process.period with Some p -> p | None -> max_int in
              let period2 = match p2.Process.period with Some p -> p | None -> max_int in
              compare period1 period2) manager_preempted.ready_queue
        | EDF ->
            List.stable_sort (fun p1 p2 -> 
              let d1 = p1.Process.arrival_time + (match p1.Process.period with Some p -> p | None -> max_int) in
              let d2 = p2.Process.arrival_time + (match p2.Process.period with Some p -> p | None -> max_int) in
              compare d1 d2) manager_preempted.ready_queue
      in
      let pcb = List.hd sorted_queue in
      let tail = List.tl sorted_queue in
      let updated_pcb = Process.update_state pcb Process.Running in
      print_endline ("Context Switch: Processo " ^ string_of_int pcb.Process.pid ^ " escalonado (+1 tempo). Política ativa: " ^ 
        match manager_preempted.policy with FCFS -> "FCFS" | Priority -> "Priority" | SJF -> "SJF" | EDF -> "EDF" | RM -> "RM");
      { manager_preempted with running = Some updated_pcb; ready_queue = tail; time = manager_preempted.time + 1 }

(* Ciclo de Fetch-Decode-Execute. *)
let rec execute_quantum manager pcb quantum_left =
  if quantum_left = 0 then
    (* O tempo esgotou, o processo FICA no CPU. Será preemptado na próxima invocação do escalonador. *)
    { manager with running = Some pcb }
  else if pcb.Process.pc >= pcb.Process.start_address + pcb.Process.size || pcb.Process.pc < pcb.Process.start_address then
    (* Proteção de memória: verifica se o PC saltou fora do bloco de memória alocado ao processo. Segurança vital. *)
    let term_pcb = Process.terminate_process pcb manager.time in
    match pcb.Process.period with
    | Some period ->
        print_endline ("Processo periódico " ^ string_of_int pcb.Process.pid ^ " concluiu execução. Agendado próximo ciclo.");
        let next_arrival = pcb.Process.arrival_time + period in
        let reset_pcb = Process.create_pcb manager.next_pid pcb.Process.ppid pcb.Process.program_name pcb.Process.start_address pcb.Process.size pcb.Process.priority next_arrival (Some period) in
        let manager_with_new = { manager with next_pid = manager.next_pid + 1; pcb_table = reset_pcb :: manager.pcb_table; new_queue = manager.new_queue @ [reset_pcb] } in
        { manager_with_new with running = None; terminated = manager_with_new.terminated @ [term_pcb] }
    | None ->
        let new_mem = if is_memory_in_use manager pcb.Process.start_address pcb.Process.pid then manager.memory else Memory.free_memory manager.memory pcb.Process.start_address pcb.Process.size in
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
        (match pcb.Process.period with
         | Some period ->
             print_endline ("Processo periódico " ^ string_of_int pcb.Process.pid ^ " agendado para o próximo ciclo.");
             let next_arrival = pcb.Process.arrival_time + period in
             let reset_pcb = Process.create_pcb manager_advanced.next_pid pcb.Process.ppid pcb.Process.program_name pcb.Process.start_address pcb.Process.size pcb.Process.priority next_arrival (Some period) in
             let manager_with_new = { manager_advanced with next_pid = manager_advanced.next_pid + 1; pcb_table = reset_pcb :: manager_advanced.pcb_table; new_queue = manager_advanced.new_queue @ [reset_pcb] } in
             { manager_with_new with running = None; terminated = manager_with_new.terminated @ [term_pcb] }
         | None ->
             let new_mem = if is_memory_in_use manager_advanced pcb.Process.start_address pcb.Process.pid then manager_advanced.memory else Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size in
             { manager_advanced with memory = new_mem; running = None; terminated = manager_advanced.terminated @ [term_pcb] })
    | Instructions.C n ->
        let pcb = Process.increment_cpu_time pcb in
        let child_start = pcb.Process.start_address in
        let child_pcb = Process.create_pcb manager_advanced.next_pid pcb.Process.pid pcb.Process.program_name child_start pcb.Process.size pcb.Process.priority manager_advanced.time pcb.Process.period in
        let child_pcb = Process.update_pc child_pcb (child_start + (pcb.Process.pc - pcb.Process.start_address) + 1) in
        let child_pcb = Process.update_value child_pcb pcb.Process.value in
        let parent_pcb = Process.update_pc pcb (pcb.Process.pc + n + 1) in
        let manager_with_child = { manager_advanced with next_pid = manager_advanced.next_pid + 1; ready_queue = manager_advanced.ready_queue @ [child_pcb]; pcb_table = child_pcb :: manager_advanced.pcb_table } in
        execute_quantum manager_with_child parent_pcb (quantum_left - 1)
    | Instructions.L filename ->
        (* Substitui o programa atual em execução por um novo programa carregado de um ficheiro *)
        let pcb = Process.increment_cpu_time pcb in
        if String.contains filename '/' || String.contains filename '\\' || String.contains filename '\000' then
          (print_endline ("Erro: Caminho inválido (Tentativa de Path Traversal/Injeção) para " ^ filename ^ ". Processo terminado.");
           let term_pcb = Process.terminate_process pcb manager_advanced.time in
           let new_mem = if is_memory_in_use manager_advanced pcb.Process.start_address pcb.Process.pid then manager_advanced.memory 
                         else Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size in
           { manager_advanced with memory = new_mem; running = None; terminated = manager_advanced.terminated @ [term_pcb] })
        else
          let manager_temp = 
             if is_memory_in_use manager_advanced pcb.Process.start_address pcb.Process.pid then manager_advanced
             else { manager_advanced with memory = Memory.free_memory manager_advanced.memory pcb.Process.start_address pcb.Process.size }
          in
          (match find_loaded_program manager_temp filename with
           | Some (start_address, size) ->
               print_endline ("Partilha de Memória: " ^ filename ^ " reutilizado (L).");
               let exec_pcb = { pcb with Process.program_name = filename; start_address = start_address; size = size; pc = start_address; value = 0 } in
               execute_quantum manager_temp exec_pcb (quantum_left - 1)
           | None ->
               let program_opt = try Some (Memory.load_program_from_file ("data/" ^ filename)) with Sys_error _ -> None in
               (match program_opt with
                | None ->
                    print_endline ("Erro: Ficheiro " ^ filename ^ " não encontrado (L). Processo terminado.");
                    let term_pcb = Process.terminate_process pcb manager_temp.time in
                    { manager_temp with running = None; terminated = manager_temp.terminated @ [term_pcb] }
                | Some (size, program) ->
                    let manager_alloc, addr_opt = allocate_with_defrag manager_temp size in
                    (match addr_opt with
                     | None ->
                         print_endline ("Erro: Sem memória para carregar " ^ filename ^ " (L). Processo terminado.");
                         let term_pcb = Process.terminate_process pcb manager_alloc.time in
                         { manager_alloc with running = None; terminated = manager_alloc.terminated @ [term_pcb] }
                     | Some start_address ->
                         let new_mem = Memory.load_program manager_alloc.memory program start_address in
                         let exec_pcb = { pcb with Process.program_name = filename; start_address = start_address; size = size; pc = start_address; value = 0 } in
                         let manager_with_prog = { manager_alloc with memory = new_mem } in
                         execute_quantum manager_with_prog exec_pcb (quantum_left - 1))))
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
