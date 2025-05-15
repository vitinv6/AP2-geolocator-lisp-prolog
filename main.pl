% DEFININDO CONEXÕES:

% conexao(Usuario1, Usuario2).
conexao(ana, bruno).
conexao(bruno, carla).
conexao(carla, daniel).
conexao(daniel, eduardo).
conexao(bruno, fabio).
conexao(fabio, eduardo).
conexao(eduardo, giovana).

% Grafo não direcionado
conectado(X, Y) :- conexao(X, Y).
conectado(X, Y) :- conexao(Y, X).


% DEFININDO A BUSCA EM LARGURA:
% menor_caminho(Origem, Destino, Caminho).
menor_caminho(Origem, Destino, Caminho) :-
    bfs([[Origem]], Destino, CaminhoInvertido),
    reverse(CaminhoInvertido, Caminho).

% Caso base: destino encontrado
bfs([[Destino|Resto]|_], Destino, [Destino|Resto]).

% Passo recursivo: expande caminhos
bfs([CaminhoAtual|Outros], Destino, CaminhoFinal) :-
    CaminhoAtual = [Cabeca|_],
    findall([Vizinho|CaminhoAtual],
            (conectado(Cabeca, Vizinho), \+ member(Vizinho, CaminhoAtual)),
            NovosCaminhos),
    append(Outros, NovosCaminhos, FilaAtualizada),
    bfs(FilaAtualizada, Destino, CaminhoFinal).


