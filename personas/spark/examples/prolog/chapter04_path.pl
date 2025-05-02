% Define direct connections between nodes
edge(a, b).
edge(a, c).
edge(b, d).
edge(c, d).
edge(d, e).

% Define a path as either a direct edge or a path with an intermediate node
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% Query: ?- path(a, e).
% Result: true
