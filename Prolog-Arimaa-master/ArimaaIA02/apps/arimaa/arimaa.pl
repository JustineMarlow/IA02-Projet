:- module(bot,
      [  get_moves/3
      ]).
:- dynamic nb_moves/1.
:- dynamic must_follow_moves/2.

get_moves([Move1,Move2,Move3,Move4], _, Board):- asserta(must_follow_moves(null, [-1,-1])),
						 possible_moves(Board, Board, [], Possible_moves_list1),
						 gen_move(Board, Possible_moves_list1, Move1, Board1),
						 possible_moves(Board1, Board1, [], Possible_moves_list2),
						 gen_move(Board1, Possible_moves_list2, Move2, Board2),
						 possible_moves(Board2, Board2, [], Possible_moves_list3),
						 gen_move(Board2, Possible_moves_list3, Move3, Board3),
						 asserta(nb_moves(3)),
						 possible_moves(Board3, Board3, [], Possible_moves_list4),
						 gen_move(Board3, Possible_moves_list4, Move4, Board4),
						 different_list(Board,Board4),
						 retractall(nb_moves(_)),
						 retractall(must_follow_moves(_,_)).

in(_,[]):-fail.
in(X,[X|_]):-!.
in(X,[_|Q]):-in(X,Q).

different_list([T1|_],[T2|_]):-T1\=T2.
different_list([T1|_],[T2|_]):-different_list(T1,T2).
different_list([X|Q1],[X|Q2]):-different_list(Q1,Q2).

adjacent(Lin1,Col1,Lin2,Col1):-Lin1<7, Lin2 is Lin1+1.
adjacent(Lin1,Col1,Lin2,Col1):-Lin1>0, Lin2 is Lin1-1.
adjacent(Lin1,Col1,Lin1,Col2):-Col1<7, Col2 is Col1+1.
adjacent(Lin1,Col1,Lin1,Col2):-Col1>0, Col2 is Col1-1.

cannot_freeze(B,[Lin1,Col1,_,silver]):-adjacent(Lin1,Col1,Lin2,Col2), in([Lin2,Col2,_,silver],B).

plus_fort(Y,rabbit):-Y\=rabbit.
plus_fort(Y,cat):-Y\=rabbit, Y\=cat.
plus_fort(Y,dog):-Y\=rabbit, Y\=cat, Y\=dog.
plus_fort(Y,horse):-Y\=rabbit, Y\=cat, Y\=dog, Y\=horse.
plus_fort(elephant,camel).

freeze(B,[Lin1,Col1,X,silver]):- \+cannot_freeze(B,[Lin1,Col1,X,silver]), adjacent(Lin1,Col1,Lin2,Col2), in([Lin2,Col2,Y,gold],B), plus_fort(Y,X).

en_avant(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin2,Col1]]):- Lin1<7, Lin2 is Lin1+1, \+in([Lin2,Col1,_,_],B), \+trap(B,[Lin2,Col1],[Lin1,Col1],silver).
en_arriere(B,[Lin1,Col1,Type,silver],[[Lin1,Col1],[Lin2,Col1]]):- Lin1>0, Type\=rabbit, Lin2 is Lin1-1, \+in([Lin2,Col1,_,_],B), \+trap(B,[Lin2,Col1],[Lin1,Col1],silver).
a_droite(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin1,Col2]]):- Col1<7, Col2 is Col1+1, \+in([Lin1,Col2,_,_],B), \+trap(B,[Lin1,Col2],[Lin1,Col1],silver).
a_gauche(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin1,Col2]]):- Col1>0, Col2 is Col1-1, \+in([Lin1,Col2,_,_],B), \+trap(B,[Lin1,Col2],[Lin1,Col1],silver).

pousser_en_avant(B,[Lin1,Col1,Type1,gold],[[Lin1,Col1],[Lin2,Col1]]):- Lin1<7, Lin2 is Lin1+1, \+in([Lin2,Col1,_,_],B), adjacent(Lin1,Col1,Lin3,Col3), in([Lin3,Col3,Type2,silver],B), \+freeze(B,[Lin3,Col3,Type2,silver]), plus_fort(Type2,Type1).
pousser_en_arriere(B,[Lin1,Col1,Type1,gold],[[Lin1,Col1],[Lin2,Col1]]):- Lin1>0, Lin2 is Lin1-1, \+in([Lin2,Col1,_,_],B), adjacent(Lin1,Col1,Lin3,Col3), in([Lin3,Col3,Type2,silver],B), \+freeze(B,[Lin3,Col3,Type2,silver]), plus_fort(Type2,Type1).
pousser_a_droite(B,[Lin1,Col1,Type1,gold],[[Lin1,Col1],[Lin1,Col2]]):- Col1<7, Col2 is Col1+1, \+in([Lin1,Col2,_,_],B), adjacent(Lin1,Col1,Lin3,Col3), in([Lin3,Col3,Type2,silver],B), \+freeze(B,[Lin3,Col3,Type2,silver]), plus_fort(Type2,Type1).
pousser_a_gauche(B,[Lin1,Col1,Type1,gold],[[Lin1,Col1],[Lin1,Col2]]):- Col1>0, Col2 is Col1-1, \+in([Lin1,Col2,_,_],B), adjacent(Lin1,Col1,Lin3,Col3), in([Lin3,Col3,Type2,silver],B), \+freeze(B,[Lin3,Col3,Type2,silver]), plus_fort(Type2,Type1).

possible_moves(Board,Board,[],[[[A,B],[X,Y]]]):- must_follow_moves(Type1,[X,Y]), Type1\=null,!, adjacent(X,Y,A,B), in([A,B,Type2,silver],Board), plus_fort(Type2,Type1), \+freeze(Board,[A,B,Type2,silver]), retract(must_follow_moves(Type1,[X,Y])).
possible_moves(_,[],_,_):-!.
possible_moves(B,[[X,Y,T,silver]|Q],ListX,ListY):- freeze(B,[X,Y,T,silver]),!, possible_moves(B,Q,ListX,ListY).
possible_moves(B,[[X,Y,T,silver]|Q],ListX,[M|ListY]):- en_avant(B,[X,Y,T,silver],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,silver]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,silver]|Q],ListX,[M|ListY]):- en_arriere(B,[X,Y,T,silver],M), T\=rabbit, \+in(M,ListX), possible_moves(B,[[X,Y,T,silver]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,silver]|Q],ListX,[M|ListY]):- a_droite(B,[X,Y,T,silver],M),  \+in(M,ListX), possible_moves(B,[[X,Y,T,silver]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,silver]|Q],ListX,[M|ListY]):- a_gauche(B,[X,Y,T,silver],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,silver]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,gold]|Q],ListX,[M|ListY]):- \+nb_moves(3), pousser_en_arriere(B,[X,Y,T,gold],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,gold]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,gold]|Q],ListX,[M|ListY]):- \+nb_moves(3), pousser_en_avant(B,[X,Y,T,gold],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,gold]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,gold]|Q],ListX,[M|ListY]):- \+nb_moves(3), pousser_a_droite(B,[X,Y,T,gold],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,gold]|Q],[M|ListX],ListY).
possible_moves(B,[[X,Y,T,gold]|Q],ListX,[M|ListY]):- \+nb_moves(3), pousser_a_gauche(B,[X,Y,T,gold],M), \+in(M,ListX), possible_moves(B,[[X,Y,T,gold]|Q],[M|ListX],ListY).
possible_moves(B,[_|Q],ListX,ListY):-possible_moves(B,Q,ListX,ListY).

adjust_board([[Lin1,Col1,X,Y]|Q], [[Lin1,Col1],[Lin2,Col2]], [[Lin2,Col2,X,Y]|Q]).
adjust_board([X|Q1], [[Lin1,Col1],[Lin2,Col2]], [X|Q2]):-adjust_board(Q1,[[Lin1,Col1],[Lin2,Col2]],Q2).

checktraps(_,[],[]).
checktraps(Board,[P|Q],R):-iscaught(P, Board), !, checktraps(Board,Q,R).
checktraps(Board,[P|Q],[P|R]):-checktraps(Board,Q,R).

generate_move(BoardX,[T|_],T,BoardY):-adjust_board(BoardX,T,BoardY),!.
generate_move(BoardX,[_|Q],M,BoardY):-generate_move(BoardX,Q,M,BoardY).

gen_move(BoardX,Moves,[[X,Y],[A,B]],BoardY):-value_moves(BoardX,Moves,Values), max_a(Values,N), indice(Values,I,N), indice(Moves,I,[[X,Y],[A,B]]), in([X,Y,Type,gold],BoardX),!, asserta(must_follow_moves(Type,[X,Y])), adjust_board(BoardX,[[X,Y],[A,B]],BoardTmp), checktraps(BoardTmp, BoardTmp, BoardY).
gen_move(BoardX,Moves,M,BoardY):-value_moves(BoardX,Moves,Values), max_a(Values,X), indice(Values,I,X), indice(Moves,I,M), adjust_board(BoardX,M,BoardTmp), checktraps(BoardTmp, BoardTmp, BoardY).

value_moves(_,[],[]).
value_moves(Board,[[[X,Y],[A,B]]|M], [100|R]) :- canwin(Board,[X,Y],[[X,Y]|[[A,B]|_]]), value_moves(Board,M,R),!.
value_moves(Board,[[[X,Y],[A,B]]|M], [90|R]) :- in([X,Y,_,gold],Board), trap(Board,[A,B],[X,Y],gold), value_moves(Board,M,R),!.
value_moves(Board,[[[X,Y],[A,B]]|M], [80|R]) :- couldwin(Board,[X,Y],[[X,Y]|[[A,B]|_]]), value_moves(Board,M,R),!.
%value_moves(Board,[[[X,Y],[_,_]]|M], [50|R]) :- in([X,Y,_,gold],Board), value_moves(Board,M,R),!.
value_moves(Board,[[[X,Y],[X2,Y]]|M], [20|R]) :- in([X,Y,rabbit,silver],Board), X2 is X+1, value_moves(Board,M,R),!.
value_moves(Board,[_|M],[10|R]) :- value_moves(Board,M,R).

indice([X|_],1,X).
indice([_|Y],N,X):- indice(Y,M,X),N is M+1.

max_a([T|List],IndiceMax):-max(List,T,IndiceMax).

max([],Max,Max).
max([T|Q],M,Max):-T>M, max(Q,T,Max).
max([T|Q],M,Max):-T=<M, max(Q,M,Max).

%trap(_,[X,Y],_,_):-different_list([X,Y],[2,2]), different_list([X,Y],[5,2]), different_list([X,Y],[2,5]), different_list([X,Y],[5,5]).
trap(B,[2,2],[L,C],Color):-adjacent(2,2,X,Y), adjacent(2,2,L,C), different_list([X,Y],[L,C]), \+in([X,Y,_,Color],B).
trap(B,[2,5],[L,C],Color):-adjacent(2,5,X,Y), adjacent(2,5,L,C), different_list([X,Y],[L,C]), \+in([X,Y,_,Color],B).
trap(B,[5,2],[L,C],Color):-adjacent(5,2,X,Y), adjacent(5,2,L,C), different_list([X,Y],[L,C]), \+in([X,Y,_,Color],B).
trap(B,[5,5],[L,C],Color):-adjacent(5,5,X,Y), adjacent(5,5,L,C), different_list([X,Y],[L,C]), \+in([X,Y,_,Color],B).
%trap(_,_,_,_).

iscaught([X,Y,_,_],_) :- different_list([X,Y],[2,2]), different_list([X,Y],[5,2]), different_list([X,Y],[2,5]), different_list([X,Y],[5,5]), !, fail.
iscaught([X,Y,_,Color],[[L,C,_,Color]|Board]) :- \+adjacent(X,Y,L,C), !,iscaught([X,Y,_,Color], Board).
%iscaught([X,Y,_,Color],[_|Board]) :- iscaught([X,Y,_,Color], Board).
iscaught(_,[]).

notrap(_,[X,Y],_,_):-different_list([X,Y],[2,2]), different_list([X,Y],[5,2]), different_list([X,Y],[2,5]), different_list([X,Y],[5,5]).
notrap(B,[X,Y],[L,C],Color):-adjacent(X,Y,X2,Y2),different_list([L,C],[X2,Y2]),in([X2,Y2,_,Color],B).

longueur([],0).
longueur([_|Q],N) :- longueur(Q,M), N is M+1.

chemin([X,Y],[X2,Y],B,[[X,Y],[X2,Y]]) :- N is X2-X,valabs(N,1),\+in([X2,Y,_,_],B),X<8,X2<8.
chemin([X,Y],[X,Y2],B,[[X,Y],[X,Y2]]) :- N is Y2-Y,valabs(N,1),\+in([X,Y2,_,_],B),Y<8,Y2<8.
chemin([A,B],[C,D],T,[[A,B]|Q]) :- A<C,X is A+1,\+in([X,B,_,_],T),chemin([X,B],[C,D],T,Q).
chemin([A,B],[C,D],T,[[A,B]|Q]) :- A>C,X is A-1,\+in([X,B,_,_],T),chemin([X,B],[C,D],T,Q).
chemin([A,B],[C,D],T,[[A,B]|Q]) :- B<D,X is B+1,\+in([X,B,_,_],T),chemin([A,X],[C,D],T,Q).
chemin([A,B],[C,D],T,[[A,B]|Q]) :- B>D,X is B-1,\+in([X,B,_,_],T),chemin([A,X],[C,D],T,Q).

valabs(X,Y) :- X<0,Y is (-1)*X,!.	%Calcul de la valeur absolue : valabs(x,|x|)
valabs(X,X).

couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,0],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,1],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,2],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,3],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,4],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,5],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,6],B,C).
couldwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,7],B,C).

canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,0],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,1],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,2],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,3],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,4],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,5],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,6],B,C), longueur(C,L), L=<4.
canwin(B,[X,Y],C):-in([X,Y,rabbit,silver],B), chemin([X,Y],[7,7],B,C), longueur(C,L), L=<4.
