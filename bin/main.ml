let () =
  let memory = Group_project.Memory.create_memory () in

  let program =
    Group_project.Memory.load_program_from_file "p1.prg"
  in

  let memory =
    Group_project.Memory.load_program memory program 0
  in

  print_endline "Simulador iniciado";

  Group_project.Memory.print_memory memory

let () =
  let plan = Group_project.Plan.load_plan "data/plan.txt" in

  print_endline "Plano carregado:";

  List.iter
    (fun (entry : Group_project.Plan.plan_entry) ->
      print_endline
        (entry.program_name ^ " chega no tempo " ^
         string_of_int entry.arrival_time))
    plan
