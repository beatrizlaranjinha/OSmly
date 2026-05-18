(* Função recursiva que processa os comandos de controlo a partir de um canal de entrada. *)
let rec control_loop manager in_channel =
  if in_channel = stdin then (print_string "> "; flush stdout);
  try
    let line = input_line in_channel in
    if in_channel <> stdin then print_endline ("> " ^ line);
    (* Formata o comando lido para maiúsculas e remove espaços. *)
    let cmd = String.trim (String.uppercase_ascii line) in
    match cmd with
    | "E" ->
        let new_manager = Group_project.Manager.command_E manager in
        control_loop new_manager in_channel
    | "I" ->
        let new_manager = Group_project.Manager.command_I manager in
        control_loop new_manager in_channel
    | "D" ->
        let new_manager = Group_project.Manager.command_D manager in
        control_loop new_manager in_channel
    | "R" ->
        let new_manager = Group_project.Manager.command_R manager in
        control_loop new_manager in_channel
    | "T" ->
        print_endline "A terminar o simulador...";
        let _ = Group_project.Manager.command_T manager in
        print_endline "Simulação terminada. Adeus!"
        (* Termina o ciclo de controlo. *)
    | "" -> control_loop manager in_channel
    | _ ->
        print_endline "Comando inválido. Use E, I, D, R, ou T.";
        control_loop manager in_channel
  with End_of_file ->
    (* Termina o simulador caso encontre o fim do ficheiro ou da entrada padrão. *)
    print_endline "\nFim da entrada. A terminar o simulador...";
    let _ = Group_project.Manager.command_T manager in
    ()

(* Função principal do programa. *)
let () =
  (* Verifica se foi passado um ficheiro de comandos como argumento (ex: ./main.exe control.txt) *)
  let in_channel =
    if Array.length Sys.argv > 1 then
      try 
        let ch = open_in Sys.argv.(1) in
        print_endline ("A ler comandos a partir do ficheiro: " ^ Sys.argv.(1));
        ch
      with Sys_error _ ->
        print_endline ("Aviso: Ficheiro " ^ Sys.argv.(1) ^ " não encontrado. Fallback para stdin.");
        stdin
    else stdin
  in

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
  print_endline "4 - EDF (Earliest Deadline First)";
  print_endline "5 - RM (Rate Monotonic)";
  if in_channel = stdin then (print_string "Opção > "; flush stdout);
  let policy = 
    try match String.trim (input_line in_channel) with
      | "1" -> Group_project.Manager.FCFS
      | "2" -> Group_project.Manager.Priority
      | "3" -> Group_project.Manager.SJF
      | "4" -> Group_project.Manager.EDF
      | "5" -> Group_project.Manager.RM
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
  control_loop manager in_channel
