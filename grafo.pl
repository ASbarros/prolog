% dependencia de dados
%Dadas as transicoes Ti e Tk, ha dependencia de dados se existe uma
% variavel v, tal que
%1. v eh definida em Ti, seja em uma acao ou evento,
%2. v eh usada em Tk, seja em uma condicao ou acao, e
%3. ha um caminho de Ti at Tk,

% => dependencia de controle
% - dois conceitos importantes
%   1. Um estado Z pos-domina um estado Y, se para cada caminho a partir de Y ate o estado final contem Z
%   2. Z pos-domina uma transicao T, se para cada caminho a partir de Y passando pela transicao T ate o estado final passar por Z
% - uma transicao Ti tem dependencia de controle em Tk se
%   1. o estado inicial de Tk nao pos-domina o estado inicial de Ti
%   2. o estado inicial de Tk pos-domina a transicao Ti


%------------- fatos -----------------------

%estado(N).
%N = numero do estado
estado(0).
estado(1).
estado(2).
estado(3).
estado(4).

%especifica qual eh o estado final
final(4).

%transicao(N, Si, Sf).
%N = numero da transicao
%Si = estado inicial
%Sf = estado final
transicao(1, 0, 1).
transicao(2, 1, 1).
transicao(3, 1, 4).
transicao(4, 1, 2).
transicao(5, 2, 3).
transicao(6, 2, 3).
transicao(7, 3, 2).
transicao(8, 2, 4).

%evento(N, T, [Vd],[Vu]).
%N = numero do evendo
%T = transicao do evento
%[Vd] = variaveis definidas no evento
%[Vu] = variaveis usadas no evento
evento(1, 1, [pin, b],[]).
evento(2, 2, [p],[]).
evento(3, 3, [p],[]).
evento(4, 4, [p],[]).
evento(5, 5, [w],[]).
evento(6, 6, [d],[]).
evento(7, 7, [],[]).
evento(8, 8, [],[]).

%condicao(N, T, [Vd],[Vu]).
%N = numero
%T = transicao
%[Vd] = variaveis definidas na condicao
%[Vu] = variaveis usados na condicao
condicao(1, 2, [], [p, pin, attempts]).
condicao(2, 3, [], [p, pin, attempts]).
condicao(3, 4, [], [p, pin]).

%acao(N, T, [Vd],[Vu]).
%N = numero
%T = transicao
%[Vd] = variaveis definidas na acao
%[Vu] = variaveis usadas na acao
acao(1, 1, [attempts], []).
acao(2, 2, [], [attempts]).
acao(3, 3, [], []).
acao(4, 4, [], []).
acao(5, 5, [], [b, w]).
acao(6, 6, [], [b, d]).
acao(7, 7, [], [b]).
acao(8, 8, [], []).

%-------------------------------------------------------------

% Encontra o caminho Solucao entre No_inicial e No_meta
tem_caminho( No_inicial, No_meta, Solucao ):-
    profundidade( [], No_inicial, No_meta, Sol_inv ),
    reverse( Sol_inv, Solucao ).

% Realiza a pesquisa em profundidade
profundidade(Caminho, No_meta, No_meta, [No_meta|Caminho]).
profundidade(Caminho, No, No_meta, Sol):-
    transicao(_, No, No1),
    not( member(No1, Caminho) ), % previne ciclos
    profundidade( [No|Caminho], No1, No_meta, Sol ).


%----------------- dependencia de dados ----------------------
% V = variavel
% Ti = transicao
% verifica se uma variavel V eh definida na transicao Ti
definida(V, Ti):-
    transicao(Ti, _, _),
    (( acao(_, Ti, La, _), member(V, La) );
    ( evento(_, Ti, Le, _), member(V, Le))).

% V = variavel
% Tk = transicao
% verifica se uma variavel V eh usada na transicao Tk
usada(V, Tk):-
    transicao(Tk, _, _),
    ( ( condicao(_, Tk, _, Lc), member(V, Lc));
    ( acao(_, Tk, _, La), member(V, La) ) ).

%funcao auxiliar para a dependencia de dados
%Ti = transicao de origem
%Tk = transicao final
dep_dados_aux(Ti, Tk, L):-
    definida(V, Ti),
    usada(V, Tk),
    transicao(Ti, No, _),
    transicao(Tk, _, No_meta),
    %findall(X, tem_caminho(No, No_meta, X), L),write(L).
    tem_caminho(No, No_meta, L).

%Ti = transicao de origem
%Tk = transicao final
dep_dados(Ti, Tk):- 
    findall(X, dep_dados_aux(Ti, Tk, X), _).


%------------------ dependencia de controle -------------------
%   1. Um estado Z pos-domina um estado Y, se para cada caminho a partir de Y ate o estado final contem Z
%Y = estado Y
%Final = estado final
% lista todos os caminhos de Y ate o estado final
lista_caminhos(Y, Final):-
    tem_caminho(Y, Final, _).

%Y = estado Y
%Z = estado Z
%Final = estado final
% lista todos os caminhos de Y ate o estado final que contem Z
lista_caminhos_2(Y, Z, L):-
    final(Final),
    tem_caminho(Y, Final, L),
    member(Z, L).

% verifica se o estado Z pos domina o estado Y
pos_domina_estado(Z, Y) :-
    final(Final), 
    findall(X, tem_caminho(Y, Final, X), L), 
    length(L, N), 
    findall(X2, lista_caminhos_2(Y, Z, X2), L2), 
    length(L2, N2),
    N == N2.

%   2. Z pos-domina uma transicao T, se para cada caminho a partir de Y passando pela transicao T ate o estado final passar por Z

% verifica se um elemento Z esta contido em uma lista, que esta dentro de outra lista. Uma lista de listas
customMember([], _).
customMember([H|Trail] , Z):-
    member(Z, H),
    percore(Trail, Z).

% verifica se o estado Z pos domina a transicao Ti
pos_domina_transicao(Z, Ti):-
    transicao(Ti, _, X),
    final(Final),
    findall(L, tem_caminho(X, Final, L), Listas),
    findall(_, customMember(Listas, Z), _).

% verifica se a transicao Tk tem dependencia de controle com a transicao Ti
dep_controle(Tk, Ti):-
    transicao(Tk, Z, _),
    transicao(Ti, Y, _),
    not(pos_domina_estado(Z, Y)),
    pos_domina_transicao(Z, Ti).
