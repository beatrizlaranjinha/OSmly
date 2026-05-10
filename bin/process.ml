(* Representa o estado atual de um processo. *)
type state =
  | Ready        (* pronto para executar *)
  | Running      (* está no CPU *)
  | Blocked      (* bloqueado *)
  | Terminated   (* terminado *)

(* Converte o estado para texto, útil para reports/debug. *)
let string_of_state state =
  match state with
  | Ready -> "Ready"
  | Running -> "Running"
  | Blocked -> "Blocked"
  | Terminated -> "Terminated"

(* PCB - Process Control Block.
   Guarda o contexto necessário para executar/retomar um processo. *)
type pcb = {
  pid : int;                    (* identificador do processo *)
  ppid : int;                   (* pid do processo pai *)
  program_name : string;        (* nome do programa .prg *)
  start_address : int;          (* posição inicial do programa na memória *)
  pc : int;                     (* program counter atual *)
  value : int;                  (* variável inteira do processo *)
  priority : int;               (* prioridade do processo *)
  arrival_time : int;           (* tempo de chegada *)
  cpu_time : int;               (* tempo total de CPU usado *)
  end_time : int option;        (* tempo de fim, se já terminou *)
  state : state;                (* estado atual do processo *)
}

(* Cria um novo PCB. *)
let create_pcb pid ppid program_name start_address priority arrival_time =
  {
    pid;
    ppid;
    program_name;
    start_address;
    pc = start_address;
    value = 0;
    priority;
    arrival_time;
    cpu_time = 0;
    end_time = None;
    state = Ready;
  }


(*executar instruções sem ser mutables*)

(* Atualiza o PC de um processo. *)
let update_pc pcb new_pc =
  { pcb with pc = new_pc }

(* Avança o PC uma instrução. *)
let increment_pc pcb =
  { pcb with pc = pcb.pc + 1 }

(* Atualiza o valor da variável inteira do processo. *)
let update_value pcb new_value =
  { pcb with value = new_value }

(* Atualiza o tempo total de CPU usado. *)
let increment_cpu_time pcb =
  { pcb with cpu_time = pcb.cpu_time + 1 }

(* Altera o estado do processo. *)
let update_state pcb new_state =
  { pcb with state = new_state }

(* Termina o processo, guardando o tempo de fim. *)
let terminate_process pcb current_time =
  { pcb with state = Terminated; end_time = Some current_time }
