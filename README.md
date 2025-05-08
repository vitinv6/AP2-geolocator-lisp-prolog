# Projeto AP2 - Aplicações em Python, Lisp e Prolog

Este repositório contém três aplicações desenvolvidas como parte da Atividade Prática 2 (AP2) da disciplina de Paradigmas de Programação.

## Aplicações

### 1. Geolocalizador Concorrente com Interface Gráfica (Python)

**Objetivo**: Fornecer a latitude e longitude de cidades brasileiras a partir de uma interface.

#### Funcionalidades:
- Interface para entrada do nome da cidade.
- Sugestão de nomes similares em caso de erro de digitação.
- Solicitação do estado quando há mais de uma cidade com o mesmo nome.
- Busca concorrente com:
  - **1 processo por região do Brasil** (N, NE, CO, S, SE)
  - **1 thread por estado** dentro de cada região.

#### Design Patterns utilizados:
- `Factory`: criação de processos e threads.
- `Observer`: encerramento de processos ao encontrar a cidade.
- `Prototype`: estrutura para reaproveitamento das threads.

#### Tecnologias:
- `tkinter` para UI
- `multiprocessing`, `threading`
- `difflib` para sugestões

---

### 2. Processamento de Dados Acadêmicos (Lisp)

**Objetivo**: Calcular a média das notas de alunos com nota maior ou igual a 7.

#### Requisitos:
- Filtro dos alunos com nota ≥ 7.
- Cálculo da média das notas filtradas.
- As funções são independentes, podendo ser aninhadas.

---

### 3. Otimização de Caminhos em Grafos (Prolog)

**Objetivo**: Determinar a menor sequência de conexões entre dois usuários em um grafo.

#### Detalhes:
- Grafo representado por dados fornecidos ou hardcoded.
- Busca da menor conexão entre dois nós do grafo.

---

## Desenvolvedores

| Nome                   | Matrícula  |
|------------------------|------------|
| **Bryan Belum**         | 2301194    |
| **Guilherme Marinho**   | 2300543    |
| **Jhoysell Chavarria**  | 2203226    |
| **Vitor Venturi**       | 2301330    |
