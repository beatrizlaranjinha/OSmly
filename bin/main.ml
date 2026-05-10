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
