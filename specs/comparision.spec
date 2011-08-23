?- 1 @< a.
true.

?- 1 == a.
false.

?- a == 'a'.
true.

?- a == "a".
false.

?- a == X.
false.

?- 2 == 1+1.
false.

?- sort(["b",'bb',b,1,b,a,[],[1,666],666],X).
X = [1, 666, [], a, b, bb, [1, 666], [98]].
