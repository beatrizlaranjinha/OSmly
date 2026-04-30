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
  mutable pc : int;             (* program counter atual *)
  mutable value : int;          (* variável inteira do processo *)
  priority : int;               (* prioridade do processo *)
  arrival_time : int;           (* tempo de chegada *)
  mutable cpu_time : int;       (* tempo total de CPU usado *)
  mutable end_time : int option;(* tempo de fim, se já terminou *)
  mutable state : state;        (* estado atual do processo *)
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
