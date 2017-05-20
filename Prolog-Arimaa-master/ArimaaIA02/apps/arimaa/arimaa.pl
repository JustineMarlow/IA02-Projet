:- module(bot,
      [  get_moves/3
      ]).

get_moves([Move1,Move2,Move3,Move4], _, Board):- possible_moves(Board, Board, [], Possible_moves_list1),
						 generate_move(Board, Possible_moves_list1, Move1, Board1),
						 possible_moves(Board1, Board1, [], Possible_moves_list2),
						 generate_move(Board1, Possible_moves_list2, Move2, Board2),
						 possible_moves(Board2, Board2, [], Possible_moves_list3),
						 generate_move(Board2, Possible_moves_list3, Move3, Board3),
						 possible_moves(Board3, Board3, [], Possible_moves_list4),
						 generate_move(Board3, Possible_moves_list4, Move4, _).
% different_list(Board,Board4).

in(_,[]):-fail.
in(X,[X|_]):-!.
in(X,[_|Q]):-in(X,Q).

different_list([T1|Q1],[T2|Q2]):-T1\=T2.
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

en_avant(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin2,Col1]]):- Lin1<7, Lin2 is Lin1+1, \+in([Lin2,Col1,_,_],B).
en_arriere(B,[Lin1,Col1,Type,silver],[[Lin1,Col1],[Lin2,Col1]]):- Lin1>0, Type\=rabbit, Lin2 is Lin1-1, \+in([Lin2,Col1,_,_],B).
a_droite(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin1,Col2]]):- Col1<7, Col2 is Col1+1, \+in([Lin1,Col2,_,_],B).
a_gauche(B,[Lin1,Col1,_,silver],[[Lin1,Col1],[Lin1,Col2]]):- Col1>0, Col2 is Col1-1, \+in([Lin1,Col2,_,_],B).

possible_moves(_,[],_,_).
possible_moves(B,[T|Q],ListX,[M|ListY]):- en_avant(B,T,M), \+in(M,ListX), \+freeze(B,T), possible_moves(B,[T|Q],[M|ListX],[M|ListY]).
possible_moves(B,[T|Q],ListX,[M|ListY]):- en_arriere(B,T,M), \+in(M,ListX), \+freeze(B,T), possible_moves(B,[T|Q],[M|ListX],[M|ListY]).
possible_moves(B,[T|Q],ListX,[M|ListY]):- a_droite(B,T,M),  \+in(M,ListX), \+freeze(B,T), possible_moves(B,[T|Q],[M|ListX],[M|ListY]).
possible_moves(B,[T|Q],ListX,[M|ListY]):- a_gauche(B,T,M), \+in(M,ListX), \+freeze(B,T), possible_moves(B,[T|Q],[M|ListX],[M|ListY]).
possible_moves(B,[_|Q],ListX,ListY):-possible_moves(B,Q,ListX,ListY).

adjust_board([[Lin1,Col1,X,Y]|Q], [[Lin1,Col1],[Lin2,Col2]], [[Lin2,Col2,X,Y]|Q]):-!.
adjust_board([X|Q1], [[Lin1,Col1],[Lin2,Col2]], [X|Q2]):-adjust_board(Q1,[[Lin1,Col1],[Lin2,Col2]],Q2).

generate_move(BoardX,[T|_],T,BoardY):-adjust_board(BoardX,T,BoardY).

% generate_move(BoardX,[T|Q],M,BoardY):-generate_move(BoardX,Q,M,BoardY).

%------------------------------------------------------------------------
%----------Prédicats-----------------------------------------------------
%------------------------------------------------------------------------
