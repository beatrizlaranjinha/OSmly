let () =
  let memory = Group_project.Memory.create_memory () in

  let manager =
    Group_project.Manager.create_manager memory
  in

  let plan =
    Group_project.Plan.load_plan "data/plan.txt"
  in

  let manager =
    List.fold_left
      Group_project.Manager.add_process_from_plan
      manager
      plan
  in

  print_endline "Simulador iniciado";
  print_endline "Processos na ready queue:";

  List.iter
    (fun pcb ->
      print_endline
        ("PID " ^
         string_of_int pcb.Group_project.Process.pid ^
         " -> " ^
         pcb.Group_project.Process.program_name))
    manager.Group_project.Manager.ready_queue
