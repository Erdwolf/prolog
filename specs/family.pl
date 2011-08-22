% Family database for testing

female(mary).
female(sandra).
female(juliet).
female(lisa).
male(peter).
male(paul).
male(dick).
male(bob).
male(harry).
male(luke).
parent(lisa, harry).
parent(peter, harry).
parent(bob,mary).
parent(juliet, paul).

parent(bob, lisa).
parent(bob, paul).
parent(juliet, mary).
parent(mary, dick).
parent(mary, sandra).
parent(juliet, lisa).
parent(harry, luke).

person(X) :- male(X).
person(X) :- female(X).

daughter(D,P) :- parent(P,D), female(D).
son(S,P) :- parent(P,S), male(S).  % abc
sibling(X,Y) :- parent(P,X), parent(P,Y), X \= Y.
sister(S,X) :- sibling(S,X), female(S).
father(F,X) :- parent(F,X), male(F).
aunt(A,X) :- parent(P,X), sister(A,P).
grandmother(GM,X) :- parent(P,X), parent(GM,P), female(GM).
cousin(C,X) :- parent(P,X), sibling(P,OA), parent(OA,C).
