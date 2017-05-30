:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,   3, [2,1,1,1,1,1]).
ejemplo(1,   4, [2,2,2,1,1,1,1]).
ejemplo(2,   5, [3,2,2,2,1,1,1,1]).
ejemplo(3,  19, [10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4, 112, [50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5, 175, [81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).
ejemplo(6, 100, [1,2,3,4,5,6,7]).

main :- 
    ejemplo(0, Big, Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl, nl,
    length(Sides, N), 
    length(RowVars, N), % get list of N prolog vars: Row coordinates of each small square
    length(ColVars, N),
    append(RowVars, ColVars, Vars),
    Vars ins 1..Big,
    insideBigSquare(Big, Sides, RowVars),
    insideBigSquare(Big, Sides, ColVars),
    nonOverLapping(Sides, RowVars, ColVars),
    label(Vars),
    displaySol(N, Sides, RowVars, ColVars), halt.

insideBigSquare(_, [], []) :- !.
insideBigSquare(Big, [S|Sides], [C|CoordVars]) :-
    C #=< Big - S + 1,
    insideBigSquare(Big, Sides, CoordVars).

nonOverLapping(Sides, RowVars, ColVars) :- 
    findall(K-S-X-Y, (nth1(K, Sides, S), nth1(K, RowVars, X), nth1(K, ColVars, Y)), Squares),
    findall((S1-X1-Y1, S2-X2-Y2), (member(K1-S1-X1-Y1, Squares), member(K2-S2-X2-Y2, Squares), K1 \= K2), Pairs),
    nonOverLapping(Pairs).

nonOverLapping([]) :- !.
nonOverLapping([(S1-X1-Y1, S2-X2-Y2)| Pairs]) :-
    trace,
    write(S1-S2), nl,
    exprOverlapping(S1-X1, S2-X2, Expr1),
    exprOverlapping(S1-Y1, S2-Y2, Expr1),
    #\ ((Expr1) #/\ (Expr2)),
    nonOverLapping(Pairs).

exprOverlapping(S1-C1, S2-C2, (Expr1 #\/ Expr2)) :-
    exprBetween(C1, C2, C1+S1, Expr1),
    exprBetween(C1, C2+S2, C1+S1, Expr2).

exprBetween(A, B, C, (A #=< B) #/\ (B #=< C)).


displaySol(N, Sides, RowVars, ColVars) :- 
    between(1, N, Row), nl, between(1, N, Col),
    nth1(K, Sides, S),
    nth1(K, RowVars, RV), RVS is RV + S - 1, between(RV, RVS, Row),
    nth1(K, ColVars, CV), CVS is CV + S - 1, between(CV, CVS, Col),
    writeSide(S), fail.
displaySol(_, _, _, _) :- nl, nl, !.

writeSide(S) :- S < 10, write('  '), write(S), !.
writeSide(S) :-         write(' ' ), write(S), !.

