%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PROBLEM 1

flatten(L, R) :-
	L = [_|_], %Make sure L is a list
	i_flatten(L, append, R).

flattenNoRepetitions(L, R) :-
	L = [_|_],
	i_flatten(L, appendNoRep([]), R).

%Reusing the code for both queries
i_flatten([H], F, R) :- !,
	i_flatten(H, F, R).
i_flatten([H|T], F, R) :- !,
	i_flatten(H, F, H_flat),
	i_flatten(T, F, T_flat),
	call(F, H_flat, T_flat, R). % Call to simply "append" or "appendNoRep"
i_flatten(X, _, [X]).

appendNoRep(_, [], [], []) :- !.
appendNoRep(Used, [], T, R) :- !,
	appendNoRep(Used, T, [], R).
appendNoRep(Used, [H|T1], T2, R) :-
	member(H, Used), !,
	appendNoRep(Used, T1, T2, R).
appendNoRep(Used, [H|T1], T2, [H|R]) :-
	Used_new = [H|Used],
	appendNoRep(Used_new, T1, T2, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PROBLEM 2
casas :- 
	Sol = [	[1,A1,B1,C1,D1,E1],
			[2,A2,B2,C2,D2,E2],
			[3,A3,B3,C3,D3,E3],
			[4,A4,B4,C4,D4,E4],
			[5,A5,B5,C5,D5,E5]],
	%   [num, color, job, pet, drink, country]
	member([_, rojo, _, _, _, peru], Sol),
	member([_, _, _, perro, _, francia], Sol),
	member([_, _, pintor, _, _, japon], Sol),
	member([_, _, _, _, ron, china], Sol),
	member([1, _, _, _, _, hungria], Sol),
	member([Verde, verde, _, _, conac, _], Sol),
	member([_, _, escultor, caracol, _, _], Sol),
	member([Actor, amarillo, actor, _, _, _], Sol),
	member([3, _, _, _, cava, _], Sol),
	member([_, _, notario, _, whisky, _], Sol),
	member([Blanca, blanco, _, _, _, _], Sol),
	Verde is Blanca - 1,
	(Caballo is Actor + 1; Caballo is Actor - 1),
	member([Caballo, _, _, caballo, _, _], Sol),
	member([Azul, azul, _, _, _, _], Sol),
	(Hungaro is Azul + 1; Hungaro is Azul - 1),
	member([Hungaro, _, _, _, _, hungria], Sol),
	member([Medico, _, medico, _, _, _], Sol),
	(Ardilla is Medico + 1; Ardilla is Medico - 1),
	member([Ardilla, _, _, ardilla, _, _], Sol),
	!, write(Sol), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	PROBLEM 3

programa(P) :- append([begin|I], [end], P), instrucciones(I).
instrucciones(I) :- instruccion(I).
instrucciones(I) :- append(I1, [';'|I2], I), instruccion(I1), instrucciones(I2).
instruccion(I) :- append(V1, ['='|A], I), variable(V1), append(V2, ['+'|V3], A), variable(V2), variable(V3).
instruccion(I) :- append([if|V1], ['='|A], I), variable(V1), append(V2, [then|I2], A), variable(V2), instrucciones(I2).
variable([x]).
variable([y]).
variable([z]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	A more efficient version of flattenNoRepetitions
/*
flattenNoRepetitions(L, R) :-
	L = [_|_],
	i_flatten(L, append, P),
	noRepetitions(P, R).

noRepetitions([H|T], [H|B]) :-
	T \= [], !,
	deleteRepetitions(H, T, A),
	noRepetitions(A, B).
noRepetitions(L, L).

deleteRepetitions(X, [], []) :- !.
deleteRepetitions(X, [X|T], R) :- !,
	deleteRepetitions(X, T, R).
deleteRepetitions(X, [H|T], [H|A]) :-
	deleteRepetitions(X, T, A).
*/
