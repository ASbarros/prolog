pertence(Elemento, [Elemento|Cauda]).
pertence(Elemento, [Cabeca|Cauda]) :-
    pertence(Elemento,Cauda).
% E = element, L = list, R = result
% e.g. add_elem_in_list ([1,2,3,4], 5, R).
add(L, E, R) :-not(member(E, L)), append(L, [E], R).
