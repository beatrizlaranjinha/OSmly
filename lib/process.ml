type state =
  | Ready
  | Running
  | Blocked
  | Terminated

let string_of_state state =
  match state with
  | Ready -> "Ready"
  | Running -> "Running"
  | Blocked -> "Blocked"
  | Terminated -> "Terminated"

type pcb = {
  pid : int;
  ppid : int;
  program_name : string;
  start_address : int;
  program_size : int;
  mutable pc : int;
  mutable value : int;
  priority : int;
  arrival_time : int;
  mutable cpu_time : int;
  mutable end_time : int option;
  mutable state : state;
}

let create_pcb pid ppid program_name start_address program_size priority arrival_time =
  {
    pid;
    ppid;
    program_name;
    start_address;
    program_size;
    pc = start_address;
    value = 0;
    priority;
    arrival_time;
    cpu_time = 0;
    end_time = None;
    state = Ready;
  }

let print_pcb pcb =
  Printf.printf
    "PID=%d PPID=%d Program=%s PC=%d Size=%d Value=%d Priority=%d Arrival=%d CPU=%d State=%s\n"
    pcb.pid
    pcb.ppid
    pcb.program_name
    pcb.pc
    pcb.program_size
    pcb.value
    pcb.priority
    pcb.arrival_time
    pcb.cpu_time
    (string_of_state pcb.state)
