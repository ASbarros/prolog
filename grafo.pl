%Dadas as transicoes Ti e Tk, ha dependencia de dados se existe uma
% variavel v, tal que
%1. v eh definida em Ti, seja em uma acao ou evento,
%2. v eh usada em Tk, seja em uma condicao ou acao, e
%3. ha um caminho de Ti at Tk,

%estado(N).
%N = numero do estado
estado(0).
estado(1).
estado(2).
estado(3).
estado(4).
estado(5).
estado(6).
estado(7).
estado(8).


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

%Ti = transicao de origem
%Tk = transicao final

definida(V, Ti):-
    transicao(Ti, _, _),
    (( acao(_, Ti, La, _), member(V, La) );
    ( evento(_, Ti, Le, _), member(V, Le))).

usada(V, Tk):-
    transicao(Tk, _, _),
    (( condicao(_, Tk, _, Lc), member(V, Lc) );
    ( acao(_, Tk, _, La), member(V, La) )).

dep_dados(Ti, Tk):- definida(V, Ti),
                    usada(V, Tk),
                    transicao(Ti,No,_),
                    transicao(Tk,_,No_meta),
                    tem_caminho(No, No_meta, _).

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

