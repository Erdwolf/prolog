line(a, b). line(c, b). line(d, a).
line(b, d). line(d, c). line(d, e).

uline(X, Y) :- line(X, Y).
uline(X, Y) :- line(Y, X).

triangle([A,B,C]) :- uline(A,B), A \= B, uline(B,C), B \= C, A \= C, uline(C,A).

tetragon([A,B,C,D]) :- uline(A,B), A \= B, uline(B,C), B \= C, A \= C, uline(C,D), C \= D, B \= D, A \= D, uline(D,A).
