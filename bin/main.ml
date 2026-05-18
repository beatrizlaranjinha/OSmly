(* Função recursiva que processa os comandos de controlo a partir da entrada padrão (stdin). *)
let rec control_loop manager =
  print_string "> ";
  flush stdout;
  try
    let line = input_line stdin in
    (* Formata o comando lido para maiúsculas e remove espaços. *)
    let cmd = String.trim (String.uppercase_ascii line) in
    match cmd with
    | "E" ->
        let new_manager = Group_project.Manager.command_E manager in
        control_loop new_manager
    | "I" ->
        let new_manager = Group_project.Manager.command_I manager in
        control_loop new_manager
    | "D" ->
        let new_manager = Group_project.Manager.command_D manager in
        control_loop new_manager
    | "R" ->
        let new_manager = Group_project.Manager.command_R manager in
        control_loop new_manager
    | "T" ->
        print_endline "A terminar o simulador...";
        let _ = Group_project.Manager.command_R manager in
        print_endline "Estatísticas globais impressas. Adeus!"
        (* Termina o ciclo de controlo. *)
    | "" -> control_loop manager
    | _ ->
        print_endline "Comando inválido. Use E, I, D, R, ou T.";
        control_loop manager
  with End_of_file ->
    (* Termina o simulador caso encontre o fim do ficheiro ou da entrada padrão. *)
    print_endline "\nFim da entrada. A terminar o simulador...";
    let _ = Group_project.Manager.command_R manager in
    ()

(* Função principal do programa. *)
let () =
  (* Inicializa o gerador de números aleatórios para o Escalonador de Longo Prazo *)
  Random.self_init ();

  (* Inicializa a memória. *)
  let memory = Group_project.Memory.create_memory () in

  (* Pergunta ao utilizador qual a política de escalonamento a usar *)
  print_endline "\n--- CONFIGURAÇÃO DO SIMULADOR ---";
  print_endline "Escolha a política de escalonamento (Short-Term):";
  print_endline "1 - FCFS (First-Come, First-Served)";
  print_endline "2 - Priority (Baseada na prioridade)";
  print_endline "3 - SJF (Shortest Job First)";
  print_endline "4 - EDF (Earliest Deadline First / Rate Monotonic)";
  print_string "Opção > ";
  flush stdout;
  let policy = 
    try match String.trim (read_line ()) with
      | "1" -> Group_project.Manager.FCFS
      | "2" -> Group_project.Manager.Priority
      | "3" -> Group_project.Manager.SJF
      | "4" -> Group_project.Manager.EDF
      | _ -> print_endline "Opção inválida. A usar Priority por defeito."; Group_project.Manager.Priority
    with End_of_file -> Group_project.Manager.Priority
  in

  (* Inicializa o gestor principal. *)
  let manager =
    Group_project.Manager.create_manager memory policy
  in

  (* Carrega o plano de execução. *)
  let plan =
    Group_project.Plan.load_plan "data/plan.txt"
  in

  (* Processa as entradas do plano e adiciona os processos ao gestor. *)
  let manager =
    List.fold_left
      Group_project.Manager.add_process_from_plan
      manager
      plan
  in

  (* Inicia o ciclo de controlo de comandos. *)
  print_endline "Simulador iniciado. Carregados processos do plano.";
  print_endline "Comandos disponíveis: E (Executar), I (Interromper), D (Desbloquear), R (Reportar), T (Terminar).";
  control_loop manager
