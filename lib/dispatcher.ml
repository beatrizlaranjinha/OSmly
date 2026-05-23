open Instructions
open Process
open Memory

type dispatch_result =
  | Continue
  | Blocked
  | Terminated
  | Fork of pcb
  | Exec of string

let dispatch_instruction memory process next_pid current_time =
  let instr = get_instruction memory process.pc in

  match instr with
  | M n ->
      process.value <- n;
      process.pc <- process.pc + 1;
      process.cpu_time <- process.cpu_time + 1;
      Continue

  | A n ->
      process.value <- process.value + n;
      process.pc <- process.pc + 1;
      process.cpu_time <- process.cpu_time + 1;
      Continue

  | S n ->
      process.value <- process.value - n;
      process.pc <- process.pc + 1;
      process.cpu_time <- process.cpu_time + 1;
      Continue

  | B ->
      process.state <- Blocked;
      process.pc <- process.pc + 1;
      process.cpu_time <- process.cpu_time + 1;
      Blocked

  | T ->
      process.state <- Terminated;
      process.cpu_time <- process.cpu_time + 1;
      process.end_time <- Some (current_time + 1);
      Terminated

  | C n ->
      let child =
        create_pcb
          next_pid
          process.pid
          process.program_name
          process.start_address
          process.program_size
          process.priority
          current_time
          process.period
          process.deadline
      in

      child.pc <- process.pc + 1;
      child.value <- process.value;

      process.pc <- process.pc + n;
      process.cpu_time <- process.cpu_time + 1;

      Fork child

  | L filename ->
      process.cpu_time <- process.cpu_time + 1;
      Exec filename

  | Empty ->
      process.state <- Terminated;
      process.end_time <- Some (current_time + 1);
      Terminated
