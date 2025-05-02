% A purely declarative approach
min(X, Y, X) :- X =< Y.
min(X, Y, Y) :- X > Y.

% Using a cut for efficiency
min_cut(X, Y, X) :- X =< Y, !.
min_cut(X, Y, Y).
