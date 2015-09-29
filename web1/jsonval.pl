%-------------JSON validation ---------------------------        
        
jsonval(In) :- atom_codes(In,L),tokenize(L,Tokens),!,json(Tokens, []).

%---------------Grammer-------------------------------

json --> ['{'],members,['}']
members --> []. 
members --> pair.
members --> pair,members.
pair --> key, [':'], value.
key --> [_].
value --> [_].





%----Tokenizer-------------------------------------------
tokenize([], []).
tokenize([C|L], L3)	:- white(C), !, skip_whites(L,L2), tokenize(L2,L3).
tokenize([C|L], [X|L3]) :- alpha(C), !,  identifier(X,[C|L],L2), tokenize(L2,L3).
tokenize([C|L], [X|L3]) :- d09(C), !, digits(X,[C|L],L2), tokenize(L2,L3).
tokenize(L, [X|L3])     :- special(X,L,L2), !, tokenize(L2,L3).

skip_whites([], []).
skip_whites([C|L], L2) :- (white(C) -> skip_whites(L,L2)
			             ; L2 = [C|L]).

white(9).  % ta
white(10). % newline
white(32). % blank

special('<>',[60,62|L],L).
special(:=,[58,61|L],L).
special(':',[58|L],L).
special(=<,[61,60|L],L).
special(>=,[62,61|L],L).
special(>,[62|L],L).
special(=,[61|L],L).
special(<,[60|L],L).
special('{',[123|L],L).
special('}',[125|L],L).
special('(',[40|L],L).
special(')',[41|L],L).
special('[',[91|L],L).
special(']',[93|L],L).
special(;,[59|L],L).
special(',',[44|L],L).
special(*,[42|L],L).
special(+,[43|L],L).
special('-->', [45,45,62|L],L).
special(-,[45|L],L).
special(.,[46|L],L).
special(/,[47|L],L).

identifier(N) --> ident(L),
		  {name(N, L)}.

ident([X|L]) --> letter(X), legits(L).
ident([X])   --> letter(X).

legits([X|W]) --> legit(X), legits(W).
legits([X])   --> legit(X).

legit(X) --> letter(X) ; digit(X).

letter(X) --> [X],  {alpha(X)}.

alpha(X) :-  X > 64,  X < 91.
alpha(X) :-  X > 96,  X < 123.

digits(N) --> digs(L), {name(N,L)}.

digs([X|L]) --> digit(X), digs(L).
digs([X]) --> digit(X).

digit(X) -->  [X],  {d09(X)}.

d09(X) :- X > 47,  X < 58.