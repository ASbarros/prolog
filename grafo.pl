% dependencia de dados
%Dadas as transicoes Ti e Tk, ha dependencia de dados se existe uma
% variavel v, tal que
%1. v eh definida em Ti, seja em uma acao ou evento,
%2. v eh usada em Tk, seja em uma condicao ou acao, e
%3. ha um caminho de Ti ate Tk,

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
transicao(9, 2, 3).

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
evento(9, 9, [],[]).

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
acao(9, 9, [], [b]).

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


%1. v eh definida em Ti, seja em uma acao ou evento,
%2. v eh usada em Tk, seja em uma condicao ou acao,
compartilha_variavel(Ti, Tk):-
    definida(V, Ti),
    usada(V, Tk).

%funcao auxiliar para a dependencia de dados
%Ti = transicao de origem
%Tk = transicao final
dep_dados_aux(Ti, Tk, L):-
    compartilha_variavel(Ti, Tk),
    transicao(Ti, No, _),
    transicao(Tk, _, No_meta),
    findall(X, tem_caminho(No, No_meta, X), L).

%Ti = transicao de origem
%Tk = transicao final
dep_dados(Ti, Tk):-
    Ti \== Tk,
    dep_dados_aux(Ti, Tk, L),
    length(L, Tamanho),
    Tamanho > 0.


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
    Z \== Y,
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
    customMember(Trail, Z).

% verifica se o estado Z pos domina a transicao Ti
pos_domina_transicao(Z, Ti):-
    transicao(Ti, _, X),
    final(Final),
    findall(L, tem_caminho(X, Final, L), Listas),
    customMember(Listas, Z).

% verifica se a transicao Tk tem dependencia de controle com a transicao Ti
dep_controle(Tk, Ti):-
    Ti \== Tk,
    transicao(Tk, Z, _),
    transicao(Ti, Y, _),
    not(pos_domina_estado(Z, Y)),
    pos_domina_transicao(Z, Ti).

%------------------ dependencia de ativacao -------------------
% adicao de uma nova transicao pode adicionar novas dependencias entre as partes existentes do sistema
% mas nao pode excluir dependencias existentes entre as partes do modelo

% existe dependencia de ativacao entre Ti e a dependencia de dados(Tj, Tk) se:
% 1. existe um caminho de Tj->Ti
% 2. Ti eh acionada
% 3. existe um caminho Ti->Tk no que diz respeito à variavel v

% Ti = transicao adicionada
% dependencia de dados(Tj, Tk)
dep_ativacao(Ti, Tj, Tk):-
    transicao(Tj, _, _),
    transicao(Tk, _, _),
    findall(X1, tem_caminho(Tj, Ti, X1), _),
    findall(X2, tem_caminho(Ti, Tk, X2), _), !.

%------------------ dependencia fantasma -------------------
% A exclusão de uma transição pode causar a eliminação de dependências associadas à transição excluída onde a transição excluída dependia de outra transição 
% a exclusão de uma transição não pode introduzir novas dependências

% 1. houver uma dependência de dados entre Tj e Ti em relação à variável v
% 2. existe um caminho claro de definição de Tj para Sb (Ti)
%    •	Sb (T) é um estado de onde T está a sair
% 3. o evento E (Ti) ocorre no estado Sb (Ti)
%    •	E (T) é um evento associado à transição T.
% 4. a condição C (Ti) é avaliada como TRUE
%    •	C (T) é uma condição de habilitação associada a transição T.
dep_fantasma(Ti, Tj):-
    transicao(Tj, _, _),
    transicao(Ti, _, _),
    dep_dados(Tj, Ti),% 1
    transicao(Ti, EstadoInicialTi, _),
    findall(X1, tem_caminho(Tj, EstadoInicialTi, X1), _),% 2
    evento(_, Ti,_,VarEvento),
    evento(_, EstadoInicialTi,VarEvento,_),% 3
    condicao(_,Ti,_,VarCondicao),
    length(VarCondicao, L),
    L > 0, % 4
    !. 


% ---------------------------------------------------------------------------------------------
% gerando o grafo de dependencias

gera_grafo_dep_dados(Lista):-
    transicao(T, _, _),
    transicao(T2, _, _),
    (member(Lista, T) ; member(Lista, T2)),
    dep_dados(T, T2),
    assertz(dependencia_dados(T, T2)).


gera_grafo_dep_controle(Lista):-
    transicao(T, _, _),
    transicao(T2, _, _),
    (member(T, Lista) ; member(T2, Lista)),
    dep_controle(T, T2),
    assertz(dependencia_controle(T, T2)).

grafo_dep(Lista):-
    findall(_, gera_grafo_dep_dados(Lista), _),
    findall(_, gera_grafo_dep_controle(Lista), _).

passo_aux(Teste, T):-
    grafo_dep(Teste),
    transicao(T1, _, _),
    transicao(T2, _, _),
    T1 \== T,
    T2 \== T,
    findall(_, retract(dependencia_dados(T1, T2)), _),
    findall(_, retract(dependencia_controle(T1, T2)), _).



% ----------------------retirando elementos repetidos em uma lista -----------------------------
retirar_todas(_, [], []).
retirar_todas(Elem, [Elem|Cauda], L):- retirar_todas(Elem, Cauda, L).
retirar_todas(Elem, [Elem1|Cauda], [Elem1|Cauda1]):-
    Elem \== Elem1,
    retirar_todas(Elem, Cauda, Cauda1).

retirar_rep([], []).
retirar_rep([Elem|Cauda], [Elem|Cauda1]):-
    retirar_todas(Elem, Cauda, Lista),
    retirar_rep(Lista, Cauda1).

% distribui(L,A,B) : distribui itens de L entre A e B
distribui([],[],[]).
distribui([X],[X],[]).
distribui([X,Y|Z],[X|A],[Y|B]) :-
    distribui(Z,A,B).

intercala([],B,B).
intercala(A,[],A).
intercala([X|A],[Y|B],[X|C]) :-
    X =< Y,
    intercala(A,[Y|B],C).
intercala([X|A],[Y|B],[Y|C]) :-
    X > Y,
    intercala([X|A],B,C).

% ordena(L,S) : ordena a lista L obtendo S
ordena([],[]).
ordena([X],[X]).
ordena([X,Y|Z],S) :-
    distribui([X,Y|Z],A,B),
    ordena(A,As),
    ordena(B,Bs),
    intercala(As,Bs,S).

compara_resultados([], [], _).
compara_resultados([H1|A], [H2|B], _) :-
    H1 == H2,
    compara_resultados(A, B, _).

% -----------------------------------------------------------------------------------------------

procura_dep_controle(T, Resp):-
    findall(_, dependencia_controle(_, T), L),
    adiciona_sem_repetir(L, Resp).



%-------------------- TESTE ADICAO --------------------------------------------------------------
% adicao_transicao([[1,4,9,7,5,7,9,7,8], [1,2,4,9,7,5,7,9,7,8], [1,2,2,4,9,7,5,7,9,7,8], [1,2,2,3]], 9, [], N).

procura_resultados_adicao([Result|Trail]):-
    findall(A, resultado_adicao(A, Result), Sol_inv),
    reverse( Sol_inv, Solucao ),
    write('casos de teste sao equivalentes: '), write(Solucao), nl,
    procura_resultados_adicao(Trail).

% Teste = caso de teste
% T = transicao adicionada
teste_adicao([], _, _, 0):-
    write('caso de teste vazio'), !.
teste_adicao(Teste, T, S, N):-
    findall(_, passo_aux(Teste, T), _),
    findall(Y, dependencia_controle(T, Y), X),
    findall(Y, dependencia_controle(Y, T), X2),
    retirar_rep(X, L),
    retirar_rep(X2, L2),
    append(L, L2, L3),
    retirar_rep([T|L3], L4),
    ordena(L4, S),
    assertz(resultado_adicao(N, S)).

% primeiro parametro = lista de casos de teste
% T = transicao adicionada
% S = solucao
% N = numero do caso de teste
testar_adicao([], _, _, 0):-!. % quando o conjunto estivar vazio
testar_adicao([Teste|Trail], T, S, N):-
    length(Trail, Length),
    Length1 is Length +1,
    teste_adicao(Teste, T, S1, Length1),
    testar_adicao(Trail, T, [S1|S], N1),
    N is N1 + 1.

adicao_transicao(Testes, T, S, N):-
    testar_adicao(Testes, T, S, N),
    findall(_, dep_ativacao(T, _, _), _),
    findall(A, resultado_adicao(_, A), L),
    retirar_rep(L, B),
    write('transicoes a serem testadas '), write(B), nl,
    procura_resultados_adicao(B).


%-------------------------------------------------- TESTE EXCLUSAO -------------------------
%  exclui_transicao([[1,4,5,7,10,9,7,8],[1,2,4,5,7,10,8]], 6, [], N).

procura_resultados_exclusao([Result|Trail]):-
    findall(A, resultado_exclusao(A, Result), Sol_inv),
    reverse( Sol_inv, Solucao ),
    write('casos de teste sao equivalentes: '), write(Solucao), nl,
    procura_resultados_exclusao(Trail).


% Teste = caso de teste
% T = transicao adicionada
teste_exclusao([], _, _, 0):-
    write('caso de teste vazio'), !.
teste_exclusao(Teste, T, S, N):-
    findall(_, passo_aux(Teste, T), _),
    findall(Y, dependencia_controle(T, Y), X),
    findall(Y, dependencia_controle(Y, T), X2),
    retirar_rep(X, L),
    retirar_rep(X2, L2),
    append(L, L2, L3),
    retirar_rep([T|L3], L4),
    ordena(L4, S),
    assertz(resultado_exclusao(N, S)).

% primeiro parametro = lista de casos de teste
% T = transicao adicionada
% S = solucao
% N = numero do caso de teste
testar_exclusao([], _, _, 0):-!. % quando o conjunto estivar vazio
testar_exclusao([Teste|Trail], T, S, N):-
    length(Trail, Length),
    Length1 is Length +1,
    teste_exclusao(Teste, T, S1, Length1),
    testar_exclusao(Trail, T, [S1|S], N1),
    N is N1 + 1.


exclui_transicao(Testes, T, S, N):-
    testar_exclusao(Testes, T, S, N),
    findall(_, dep_fantasma(T, _), _),
    findall(A, resultado_exclusao(_, A), L),
    retirar_rep(L, B),
    write('transicoes a serem testadas '), write(B), nl,
    procura_resultados_exclusao(B).