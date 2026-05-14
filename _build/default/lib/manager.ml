(* manager.ml *)

type manager = {
  time : int;                         (* tempo atual do simulador *)
  memory : Memory.memory;             (* memória simulada *)
  pcb_table : Process.pcb list;       (* tabela de processos *)
  ready_queue : Process.pcb list;     (* fila de processos prontos *)
  blocked_queue : Process.pcb list;   (* fila de processos bloqueados *)
  terminated : Process.pcb list;      (* processos terminados *)
  next_pid : int;                     (* próximo PID disponível *)
}

let create_manager memory =
  {
    time = 0;
    memory;
    pcb_table = [];
    ready_queue = [];
    blocked_queue = [];
    terminated = [];
    next_pid = 1;
  }

(* Cria um processo a partir de uma entrada do plano e coloca-o na fila de prontos. *)
let add_process_from_plan manager plan_entry =
  let program =
    Memory.load_program_from_file ("data/" ^ plan_entry.Plan.program_name)
  in

  let start_address = 0 in

  let memory =
    Memory.load_program manager.memory program start_address
  in

  let pcb =
    Process.create_pcb
      manager.next_pid
      0
      plan_entry.Plan.program_name
      start_address
      1
      plan_entry.Plan.arrival_time
  in

  {
    manager with
    memory = memory;
    pcb_table = pcb :: manager.pcb_table;
    ready_queue = manager.ready_queue @ [pcb];
    next_pid = manager.next_pid + 1;
  }
