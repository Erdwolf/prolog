union([],Xs,Xs).
union([X|Xs],Ys,[X|Zs]) :- not(member(X,Ys)),union(Xs,Ys,Zs).
union([X|Xs],Ys,Zs) :- member(X,Ys),!,union(Xs,Ys,Zs).
