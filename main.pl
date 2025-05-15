% DEFININDO CONEXÕES:

conexao(1, 2).
conexao(1, 3).
conexao(1, 10).
conexao(2, 4).
conexao(3, 8).
conexao(3, 9).
conexao(4, 8).
conexao(4, 6).
conexao(5, 9).
conexao(5, 7).
conexao(6, 10).
conexao(7, 10).
conexao(7, 9).
conexao(8, 10).


% Grafo não direcionado
conectado(X, Y) :- conexao(X, Y).
conectado(X, Y) :- conexao(Y, X).


% DEFININDO A BUSCA EM LARGURA:
bfs(Inicio, Fim, Caminho) :-
    bfs_aux([[Inicio]], Fim, Caminho).

% CASO: ENCONTROU O DESTINO:
bfs_aux([[Fim | T] | _], Fim, Caminho) :-
    reverse([Fim | T], Caminho).

% CASO: EXPLORANDO A FILA:
bfs_aux([[Atual | T] | Fila], Fim, Caminho) :-
    findall([Prox, Atual | T],
            (conectado(Atual, Prox),
             \+ member(Prox, [Atual | T])),
            NovosCaminhos),
    append(Fila, NovosCaminhos, FilaAtualizada),
    bfs_aux(FilaAtualizada, Fim, Caminho).

