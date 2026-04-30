OSmly — Simulador de Gestão de Processos, Escalonamento e Gestão de Memória em OCaml

# Plano de Desenvolvimento: Simulador de Sistema Operativo em OCaml

Este documento detalha o planeamento de execução do projeto, dividido por fases, sprints e atribuições individuais.

---

## Fase 1: Fundações, Infraestrutura e Estruturas de Dados (Sprint 1)

### 🛠️ Tarefa 1.1: Setup do Repositório, CI/CD e Build System
* **Atribuição:** Dev 1
* **Ordem / Fase:** Fase 1 (Blocker para as restantes)
* **Finalidade Explícita:** Garantir uma infraestrutura de integração contínua imaculada e controlo de versões, estabelecendo o fluxo de trabalho da equipa.
* **Texto Orientador (Briefing de Execução):**
    Dev 1, a fundação começa contigo. Preciso que cries o repositório no GitLab/GitHub e partilhes os acessos com a equipa e professores. Configura o ambiente de compilação utilizando estritamente o `dune` para OCaml. Garante que o projeto compila e executa nativamente num sistema POSIX (Linux/MacOS/WSL) a partir da linha de comandos.
* **Definition of Done (DoD):**
    - [ x] Repositório criado e acessível.
    - [ x] `dune` a compilar um "Hello World" em OCaml com sucesso.
    - [ x] Pipeline de CI básico (GitHub Actions/GitLab CI) a validar o build em ambiente Linux.
    - [ x] Uso obrigatório de commits para histórico de desenvolvimento.

### 🧠 Tarefa 1.2: Desenho de Tipos Core: PCB e Modelo de Memória
* **Atribuição:** Dev 2
* **Ordem / Fase:** Fase 1
* **Finalidade Explícita:** Criar as abstrações matemáticas e os Tipos de Dados Algébricos (ADTs) em OCaml que vão representar o estado do sistema.
* **Texto Orientador (Briefing de Execução):**
    Dev 2, vais modelar o cérebro da nossa simulação. Implementa o Process Control Block (PCB) como um `Record` em OCaml, incluindo: nome, start, PID (o gestor é o 0), PPID, PC, estado e prioridade. Cria a estrutura de memória: um array único de 1000 posições onde cada posição guarda uma instrução. 
    *Atenção:* OCaml é funcional, mas para o array de memória deves usar `Array.make` para performance.
* **Definition of Done (DoD):**
    - [ ] Módulos OCaml para PCB e Memória criados.
    - [ ] Testes unitários a validar a inicialização das estruturas.

### 📥 Tarefa 1.3: Parsing de Ficheiros e Comandos
* **Atribuição:** Dev 3
* **Ordem / Fase:** Fase 1
* **Finalidade Explícita:** Garantir que o simulador consegue ingerir dados externos (programas e comandos) de forma resiliente.
* **Texto Orientador (Briefing de Execução):**
    Dev 3, o teu foco é o I/O. Cria parsers para:
    1.  `plan.txt`: lista de programas, tempos de chegada e prioridades.
    2.  `control.txt` (ou stdin): comandos E, I, D, R, T.
    3.  Ficheiros `.prg`: as 7 instruções (ex: M n, L filename).
* **Definition of Done (DoD):**
    - [ ] Módulo de parsing funcional.
    - [ ] Conversão de strings em tipos OCaml sem crashes ou exceções não tratadas.

---

## Fase 2: Motor de Execução e Gestão de Estado (Sprint 2)

### ⚙️ Tarefa 2.1: Máquina Virtual (Motor de Instruções)
* **Atribuição:** Dev 1
* **Ordem / Fase:** Fase 2 (Depende do Parsing)
* **Finalidade Explícita:** Dar vida aos processos, permitindo que as instruções afetem o estado das variáveis e da memória.
* **Texto Orientador (Briefing de Execução):**
    Dev 1, implementa a lógica para as 7 instruções: M (mudar), A (adicionar), S (subtrair), B (bloquear), T (terminar), C (clone/fork), L (carregar novo programa). Lembra-te que o clone (C) faz o pai saltar *n* instruções e o filho começa logo a seguir. A instrução L limpa a memória anterior.
* **Definition of Done (DoD):**
    - [ ] Função de execução que recebe uma instrução e o PCB, retornando o PCB mutado corretamente.

### 🎼 Tarefa 2.2: Comutação de Contexto e Ciclo Principal (Manager)
* **Atribuição:** Dev 2
* **Ordem / Fase:** Fase 2 (Depende dos Tipos Core)
* **Finalidade Explícita:** Orquestrar a simulação através do tempo e gerir o CPU de forma preemptiva.
* **Texto Orientador (Briefing de Execução):**
    Dev 2, vais construir o "Maestro". Implementa: Tempo, CPU, PcbTabela, filas de Prontos/Bloqueados e o RunningState. Cria a lógica de comutação de contexto (salvaguarda e carregamento do PC). Executa o ciclo baseado no Time Quantum (comando E).
* **Definition of Done (DoD):**
    - [ ] Ciclo principal funcional com avanço de tempo.
    - [ ] Comutação de contexto sem perda de dados.

### 🧹 Tarefa 2.3: Gestor de Memória Avançado (Alocação e Desfragmentação)
* **Atribuição:** Dev 3
* **Ordem / Fase:** Fase 2 (Depende dos Tipos Core)
* **Finalidade Explícita:** Garantir que o sistema sobrevive a longo prazo sem esgotar o array de 1000 instruções.
* **Texto Orientador (Briefing de Execução):**
    Dev 3, precisamos de um "Garbage Collector" rudimentar. Ao terminar (T) ou carregar (L), anula as posições de memória. Implementa a verificação de espaço antes de colocar em Ready e um mecanismo de desfragmentação (compactação do array) se necessário.
* **Definition of Done (DoD):**
    - [ ] Carregamento dinâmico de processos.
    - [ ] Libertação de slots por processos mortos.
    - [ ] Algoritmo de desfragmentação funcional.

---

## Fase 3: Escalonadores e Relatórios (Sprint 3)

### 📊 Tarefa 3.1: Escalonadores de Curto Prazo I (Milestone 1)
* **Atribuição:** Dev 1
* **Ordem / Fase:** Fase 3
* **Finalidade Explícita:** Garantir a entrega do Milestone 1 e implementar algoritmos clássicos.
* **Texto Orientador (Briefing de Execução):**
    Dev 1, implementa FCFS (First-Come, First-Served) e SJF (Shortest Job First). No SJF, aproveita o facto de não haver loops para saber a duração exata do processo.
* **Definition of Done (DoD):**
    - [ ] FCFS e SJF funcionais e selecionáveis.

### ⏱️ Tarefa 3.2: Escalonadores de Curto Prazo II (Tempo Real)
* **Atribuição:** Dev 2
* **Ordem / Fase:** Fase 3
* **Finalidade Explícita:** Adicionar algoritmos complexos preemptivos e de tempo real.
* **Texto Orientador (Briefing de Execução):**
    Dev 2, expande a interface do Dev 1. Implementa o escalonamento por Prioridade e os algoritmos de tempo real: Rate Monotonic (RM) e Earliest Deadline First (EDF).
* **Definition of Done (DoD):**
    - [ ] Suporte a Priority, RM e EDF com opção de preempção.

### 📈 Tarefa 3.3: Escalonador de Longo Prazo e Sistema de Relatórios
* **Atribuição:** Dev 3
* **Ordem / Fase:** Fase 3
* **Finalidade Explícita:** Dar visibilidade ao sistema e gerir o regresso de processos bloqueados.
* **Texto Orientador (Briefing de Execução):**
    Dev 3, cria o escalonador de longo prazo (comando D) que usa probabilidade (rand) para desbloquear processos. Implementa o comando R (Report) para imprimir o estado atual e as estatísticas globais (turnaround, CPU usage) no comando T final.
* **Definition of Done (DoD):**
    - [ ] Logs formatados de acordo com o enunciado.
    - [ ] Processos desbloqueados probabilisticamente.

---

## Fase 4: QA, Documentação e Milestone 2 (Sprint 4)

### 🚀 Tarefa 4.1: Integração Final, Testes End-to-End e Relatório
* **Atribuição:** Dev 1, Dev 2 e Dev 3 (Trabalho Conjunto)
* **Ordem / Fase:** Fase 4
* **Finalidade Explícita:** Fechar o projeto com a excelência técnica exigida para nota máxima.
* **Texto Orientador (Briefing de Execução):**
    Equipa, reta final para o Milestone 2. Dev 1 e 2 testam concorrência alta (forks/execs simultâneos). Dev 3 coordena o relatório em PDF (explicação de algoritmos e exemplos de execução, sem código excessivo).
* **Definition of Done (DoD):**
    - [ ] Ficheiro `.zip`/`.tar` com código, `dune` e PDF prontos.
    - [ ] Professores adicionados ao repositório GitLab.
    - [ ] Contribuições individuais visíveis no histórico de commits.
