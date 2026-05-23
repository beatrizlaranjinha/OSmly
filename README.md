# OSmly — Simulador de Gestão de Processos em OCaml

OSmly é um simulador de gestão de processos desenvolvido em OCaml no âmbito da unidade curricular de Sistemas Operativos.

O projeto simula funcionalidades fundamentais de um sistema operativo, incluindo:
- escalonamento de processos;
- gestão de memória;
- criação e terminação de processos;
- bloqueio e desbloqueio;
- execução baseada em time quantum;
- mecanismos fork/exec;
- algoritmos clássicos e de tempo real.

---

# Funcionalidades

## Escalonamento
- FCFS (First Come First Served)
- Priority Scheduling
- SJFS (Shortest Job First Scheduling)
- RM (Rate Monotonic)
- EDF (Earliest Deadline First)

## Gestão de Processos
- PCB Table
- Ready Queue
- Running Process
- Blocked Queue
- Terminated Queue

## Gestão de Memória
- Memória simulada com 1000 posições
- Carregamento de programas
- Procura de espaço livre
- Libertação de memória

## Instruções Suportadas
- `M n` — altera valor
- `A n` — soma valor
- `S n` — subtrai valor
- `B` — bloqueia processo
- `T` — termina processo
- `C n` — cria processo filho (fork)
- `L file` — substitui programa atual (exec)

---

# Estrutura do Projeto

```text
lib/
├── dispatcher.ml
├── instructions.ml
├── manager.ml
├── memory.ml
├── plan.ml
├── process.ml
├── report.ml
└── scheduler.ml

data/
├── plan.txt
├── control.txt
└── *.prg
```

---

# Como Compilar

## Build

```bash
dune build
```

## Rebuild

```bash
dune clean && dune build
```

## Clean

```bash
dune clean
```

---

# Como Executar

## FCFS

```bash
dune exec group_project -- fcfs
```

## Priority Scheduling

```bash
dune exec group_project -- priority
```

## SJFS

```bash
dune exec group_project -- sjfs
```

## Rate Monotonic

```bash
dune exec group_project -- rm
```

## Earliest Deadline First

```bash
dune exec group_project -- edf
```

---

# Ficheiros de Entrada

## plan.txt

Define:
- programas;
- tempo de chegada;
- prioridade;
- período;
- deadline.

Exemplo:

```text
p1.prg 0 5 5 20
p2.prg 0 1 3 10
p3.prg 0 3 8 15
```

---

## control.txt

Controla a execução do simulador.

Comandos disponíveis:
- `E` — executar
- `D` — desbloquear processo
- `R` — relatório do sistema
- `T` — terminar simulador

---

# Exemplo de Execução

```bash
dune exec group_project -- priority
```

---

# Tecnologias Utilizadas

- OCaml
- Dune
- LaTeX

---

# Repositório

```text
https://github.com/beatrizlaranjinha/OSmly/
```
