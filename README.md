# OSmly — Simulador de Gestão de Processos em OCaml

OSmly é um simulador de gestão de processos desenvolvido em OCaml no âmbito da unidade curricular de Sistemas Operativos.


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


Como Compilar

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

Como Executar

##FCFS

```bash
dune exec group_project -- fcfs
```

##Priority Scheduling

```bash
dune exec group_project -- priority
```

##SJFS

```bash
dune exec group_project -- sjfs
```

##Rate Monotonic

```bash
dune exec group_project -- rm
```

##Earliest Deadline First

```bash
dune exec group_project -- edf
```



