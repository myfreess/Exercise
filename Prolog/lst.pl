filter(_,[],[]).
filter(P,[E|T],[E|RS]) :- call(P,E), !, filter(P,T,RS).
filter(P,[_|T],RS) :- filter(P,T,RS).

% 好耶!

foldl(_,Acc,[],Acc).
foldl(P,Acc,[X|XS],Res) :- call(P,Acc,X,R), foldl(P,R,XS,Res).


