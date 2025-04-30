% Facts
parent(john, mary).    % John is a parent of Mary
parent(john, tom).     % John is a parent of Tom
parent(mary, ann).     % Mary is a parent of Ann
parent(mary, pat).     % Mary is a parent of Pat
parent(tom, jim).      % Tom is a parent of Jim

% Rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).

% Query example (would be entered at the Prolog prompt)
% ?- grandparent(john, Who).
% Result: Who = ann ; Who = pat ; Who = jim
