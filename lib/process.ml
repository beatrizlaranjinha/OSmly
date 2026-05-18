(* Estados possíveis de um processo. *)
type state =
  | Ready        (* Pronto a executar *)
  | Running      (* Em execução no CPU *)
  | Blocked      (* Bloqueado a aguardar um evento *)
  | Terminated   (* Execução terminada *)

(* Converte um estado numa string. *)
let string_of_state state =
  match state with
  | Ready -> "Ready"
  | Running -> "Running"
  | Blocked -> "Blocked"
  | Terminated -> "Terminated"

(* Estrutura do Process Control Block (PCB). *)
type pcb = {
  pid : int;                    (* Identificador do processo (PID) *)
  ppid : int;                   (* Identificador do processo pai (PPID) *)
  program_name : string;        (* Nome do ficheiro executável *)
  start_address : int;          (* Endereço inicial na memória *)
  size : int;                   (* Dimensão do programa na memória *)
  pc : int;                     (* Apontador para a próxima instrução (PC) *)
  value : int;                  (* Variável de estado do processo *)
  priority : int;               (* Nível de prioridade *)
  arrival_time : int;           (* Tempo de submissão do processo *)
  cpu_time : int;               (* Tempo total gasto no CPU *)
  end_time : int option;        (* Tempo em que a execução terminou *)
  state : state;                (* Estado atual do processo *)
  period : int option;          (* Período para os processos de tempo real *)
}

(* Cria um novo PCB com os dados iniciais. *)
let create_pcb pid ppid program_name start_address size priority arrival_time period =
  {
    pid;
    ppid;
    program_name;
    start_address;
    size;
    pc = start_address; (* O PC é inicializado no endereço inicial *)
    value = 0;          (* Valor inicial da variável *)
    priority;
    arrival_time;
    cpu_time = 0;
    end_time = None;
    state = Ready;      (* O estado inicial é Ready *)
    period;
  }

(* Funções auxiliares para atualização do PCB de forma funcional. *)

(* Atualiza o valor do PC. *)
let update_pc pcb new_pc =
  { pcb with pc = new_pc }

(* Incrementa o PC numa posição. *)
let increment_pc pcb =
  { pcb with pc = pcb.pc + 1 }

(* Atualiza o valor da variável do processo. *)
let update_value pcb new_value =
  { pcb with value = new_value }

(* Incrementa o tempo gasto no CPU. *)
let increment_cpu_time pcb =
  { pcb with cpu_time = pcb.cpu_time + 1 }

(* Atualiza o estado do processo. *)
let update_state pcb new_state =
  { pcb with state = new_state }

(* Altera o estado para Terminated e regista o tempo final. *)
let terminate_process pcb current_time =
  { pcb with state = Terminated; end_time = Some current_time }
