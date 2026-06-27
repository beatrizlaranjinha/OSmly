# OSmly — Simulador de Gestão de Processos em OCaml

OSmly é um simulador de gestão de processos desenvolvido em OCaml no âmbito da unidade curricular de Sistemas Operativos.


---

# Funcionalidades

Escalonamento
Gestão de Processos
Gestão de Memória


Instruções Suportadas
- `M n` — altera valor
- `A n` — soma valor
- `S n` — subtrai valor
- `B` — bloqueia processo
- `T` — termina processo
- `C n` — cria processo filho (fork)
- `L file` — substitui programa atual (exec)


Como Compilar

Build

```bash
dune build
```



Como Executar

FCFS

```bash
dune exec group_project -- fcfs
```

Priority Scheduling

```bash
dune exec group_project -- priority
```

SJFS

```bash
dune exec group_project -- sjfs
```

Rate Monotonic

```bash
dune exec group_project -- rm
```

Earliest Deadline First

```bash
dune exec group_project -- edf
```



