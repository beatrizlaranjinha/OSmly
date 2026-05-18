(* sheduler.ml *)

open Manager 

(* Escalonador FCFS *)
let schedule_fcfs manager =
  match manager.running, manager.ready_queue with
  | Some _, _ -> 
      (* Já há um processo a correr. *)
      manager
  | None, [] -> 
      (* Nenhum processo pronto e CPU livre *)
      manager
  | None, next_process :: remaining_ready ->
      (* Retira o primeiro da fila, muda o estado para Running e aloca ao CPU *)
      let running_process = Process.update_state next_process Process.Running in
      { manager with 
        running = Some running_process; 
        ready_queue = remaining_ready }

(* Escalonador por Prioridade não Preemptivo *)
let schedule_priority manager =
  match manager.running with
  | Some _ -> manager
  | None ->
      match manager.ready_queue with
      | [] -> manager
      | queue ->
          let sorted_queue = 
            List.sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) queue 
          in
          let next_process = List.hd sorted_queue in
          let remaining_ready = List.tl sorted_queue in
          let running_process = Process.update_state next_process Process.Running in
          { manager with 
            running = Some running_process; 
            ready_queue = remaining_ready }


(* Escalonador por Prioridade Preemptivo*)
let schedule_priority manager =
  match manager.ready_queue with
  | [] -> manager (* Não há processos à espera, mantemos o estado atual *)
  | queue ->
      (* Ordenar a fila para ter o processo mais prioritário no início *)
      let sorted_queue = 
        List.sort (fun p1 p2 -> compare p1.Process.priority p2.Process.priority) queue 
      in
      let top_ready = List.hd sorted_queue in
      let remaining_ready = List.tl sorted_queue in

      match manager.running with
      | None ->
          (* O CPU estava livre, avança o processo mais prioritário *)
          let new_running = Process.update_state top_ready Process.Running in
          { manager with 
            running = Some new_running; 
            ready_queue = remaining_ready }
            
      | Some current_running ->
          (* O CPU está ocupado: vamos comparar prioridades *)
          if top_ready.Process.priority < current_running.Process.priority then
            let preempted_process = Process.update_state current_running Process.Ready in
            let new_running = Process.update_state top_ready Process.Running in
            
            { manager with 
              running = Some new_running; 
              (* O processo interrompido volta para a fila de prontos *)
              ready_queue = preempted_process :: remaining_ready }
          else
            (* O processo atual continua a ser o mais prioritário. Não fazemos nada. *)
            manager


