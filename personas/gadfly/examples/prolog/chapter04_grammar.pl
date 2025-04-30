sentence(S) --> noun_phrase(NP), verb_phrase(VP).
noun_phrase(NP) --> determiner(D), noun(N).
verb_phrase(VP) --> verb(V).
verb_phrase(VP) --> verb(V), noun_phrase(NP).

determiner(the).
determiner(a).
noun(cat).
noun(dog).
verb(sees).
verb(chases).

% Query: ?- sentence([the, cat, sees, the, dog], []).
% Result: true
