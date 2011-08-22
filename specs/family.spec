?- daughter(X, bob).
X = mary ;
X = lisa ;
false.

?- son(_,X), daughter(_,X).
X = juliet ;
X = juliet ;
X = bob ;
X = bob ;
X = mary ;
false.

?- person(X), \+ parent(juliet,X).
X = peter ;
X = dick ;
X = bob ;
X = harry ;
X = luke ;
X = sandra ;
X = juliet ;
false.

?- parent(M,X), female(M), M \= juliet.
M = lisa,
X = harry ;
M = mary,
X = dick ;
M = mary,
X = sandra ;
false.

?- sister(X,Y).
X = mary,
Y = lisa ;
X = mary,
Y = paul ;
X = lisa,
Y = mary ;
X = lisa,
Y = paul ;
X = mary,
Y = paul ;
X = mary,
Y = lisa ;
X = sandra,
Y = dick ;
X = lisa,
Y = paul ;
X = lisa,
Y = mary ;
false.

?- father(X,Y).
X = peter,
Y = harry ;
X = bob,
Y = mary ;
X = bob,
Y = lisa ;
X = bob,
Y = paul ;
X = harry,
Y = luke.

?- aunt(X,Y).
X = mary,
Y = harry ;
X = mary,
Y = harry ;
X = lisa,
Y = dick ;
X = lisa,
Y = dick ;
X = lisa,
Y = sandra ;
X = lisa,
Y = sandra ;
false.

?- grandmother(X,Y).
X = juliet,
Y = harry ;
X = juliet,
Y = dick ;
X = juliet,
Y = sandra ;
X = lisa,
Y = luke ;
false.

?- cousin(X,Y).
X = dick,
Y = harry ;
X = sandra,
Y = harry ;
X = dick,
Y = harry ;
X = sandra,
Y = harry ;
X = harry,
Y = dick ;
X = harry,
Y = dick ;
X = harry,
Y = sandra ;
X = harry,
Y = sandra ;
false.
