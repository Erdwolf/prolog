?- a(a;b,c;d) =.. L.
L = [a, (a;b), (c;d)].

?- (a;b,c;d) =.. L.
L = [;, a, (b, c;d)].

?- true.
true.

?- false.
false.

?- fail.
false.

?- O_O = o_o.
O_O = o_o.

?- (a,,,b) =.. L.
L = [',', a, ((','), b)].

?- 'foo' = foo.
true.
