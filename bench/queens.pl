% Find all solutions of an 11 by 11 board.
% The \+ with the fail is a trick to make it find all solutions.
queensBenchmark :- time(\+ (queens(11, _Qs), fail)).
queens(N,Qs) :- rangeList(1,N,Ns), queens3(Ns,[],Qs).

queens3(UnplacedQs, SafeQs, Qs) :-
  selectq(Q, UnplacedQs, UnplacedQs1),
  \+ attack(Q,SafeQs),
  queens3(UnplacedQs1,[Q|SafeQs],Qs).
queens3([],Qs,Qs).

attack(X,Xs) :- attack3(X, 1, Xs).

attack3(X,N,[Y|_]) :- X =:= Y+N ; X =:= Y-N.
attack3(X,N,[_|Ys]) :- N1 is N+1, attack3(X,N1,Ys).

rangeList(M,N,[M]) :- M >= N, !.
rangeList(M,N,[M|Tail]) :- M1 is M+1, rangeList(M1,N,Tail).

selectq(X,[X|Xs],Xs).  
selectq(X,[Y|Ys],[Y|Zs]) :- selectq(X,Ys,Zs). 
