% Simple Interactive Program Verifier
% by Bharat Jayaraman
% State Univ of New York at Buffalo


:- op(700, xfx, ':=').
:- op(650, xfx, '=<').
:- op(650, xfx, '>=').
:- op(650, xfx, '<>').
:- op(650, xfx, '>').
:- op(650, xfx, '<').
:- op(600, yfx, 'and').

% _______ TOP-LEVEL DRIVER ______________________________________________

verify(File) :-
	open(File,read,Stream),
	lex(Stream,Tokens),
	parse(Tokens,Triple),
	prove(Triple),
	close(Stream).



% ___________GRAMMAR RULES TinyPL ___________________________________


parse(Tokens,Triple) :- program(Triple,Tokens, []), !.

program(triple(Pre,ParseTree,Post))
		--> [input], bexpr(Pre),
		    [output], bexpr(Post),
		    [program], cmpdstmt(ParseTree),
		    ['.'].

% cmpdstmt AND OTHER STATEMENTS

cmpdstmt(S)     --> ['{'], stmts(S), ['}'].

stmts((S1 ; S)) --> stmt(S1), stmts(S).
stmts(S)        --> stmt(S).

stmt(S)         --> assign(S), [';'].
stmt(S)         --> ifthenelse(S), [';'].
stmt(S)         --> while(S), [';'].
stmt(S)		--> for(S),[';'].

assign((L = R)) --> var(L), [=], expr(R).

ifthenelse(if(B,T))   --> [if], ['('], expr(B), [')'], cmpdstmt(T).
ifthenelse(if(B,T,E)) --> [if], ['('], expr(B), [')'], [then], 
					cmpdstmt(T), [else], cmpdstmt(E).

while(whl(B,W)) --> [while], ['('], bexpr(B), [')'], cmpdstmt(W).
for(whl(B,W;S1)) --> [for],['('],assign(S) ,[';'],bexpr(B),[';'],assign(S1),[')'],cmpdstmt(W).



writelist([]).
writelist([H|T]) :- write(H), nl, writelist(T).

% EXPRESSION GRAMMAR (expr and bexpr)

bexpr(T) --> expr(T).

expr(T)   --> term(T1), term2(T1, T).

term2(T1,T)  --> [OP], {operator(OP)}, term(T2), {T3 =.. [OP,T1,T2]},
		 term2(T3,T).
term2(T,T)  --> [].

term(true) --> [true].
term(X) --> int(X).
term(T) --> ['('], expr(T), [')'].
term(not(X)) --> [not], expr(X).
term(T) --> var(X), ['('], expr(T1),[':'], expr(T2), [')'], {T =.. [X,T1:T2]}.
term(T) --> var(X), ['['], exprlist(L), [']'], {T =.. [X|L]}.
term(T) --> var(X), ['('], exprlist(L), [')'], {T =.. [X|L]}.
term(X) --> var(X).

int(X)  --> [id(X)].
int(X)  --> [num(X)].

var(X) --> [id(X)].

exprlist([])   --> [].
exprlist([E])   --> expr(E).
exprlist([E|L]) --> expr(E), [','], exprlist(L).

operator(+).
operator(-).
operator(/).
operator(*).
operator('and').
operator('-->').
operator(>=).
operator(=<).
operator(<>).
operator(=).
operator(>).
operator(<).

% _____________________________PROVER______________________________________
%


prove(triple(Pre,Stmt,Post)) :-
        (prove(Pre,Stmt,Post)
          -> nl, write('The program is verified for partial correctness!')
          ;  nl, write('The proof failed.')
        ), nl, nl.

prove(Pre,Stmt,Post) :-
	wp(WP,Stmt,Post), !,
	theorem((Pre --> WP)).


% ________ The wp predicate: Weakest Preconditions _____________________


wp(Pre, (X = Y), Post) :-
	subst(Post, X, Y, Pre).

wp(Pre,	(S ; REST), Post) :-
	wp(Inter, REST, Post),
	wp(Pre, S, Inter).

wp(((X --> TR) and (not(X) --> Post)), if(X, Y), Post) :-
	wp(TR, Y, Post).

wp((((X --> TR) and (not(X) --> FA))), if(X, Y, Z), Post) :-
	wp(TR, Y, Post),
	wp(FA, Z, Post).

wp(INV, whl(B, S), Post) :-
	nl, write('Enter loop invariant for: while '),
	    write('('), write(B), write(') '),
	    write('{'), write(S), write('}'), nl,nl,
	read(INV),
	wp(Q, S, INV), !,
	theorem(((B and INV) --> Q)),
	theorem(((not(B) and INV) --> Post)).


% _____________________________________________

theorem(T) :- nl, write('Is the following true? '), nl, nl,
	      pprin(T,1), !,
	      nl, nl,
	      write('Enter true./false.: '), read(Inp),
	      Inp == true.

% __________ Substitution:  P[V <- E] = Q _____

subst(V,V,E,E) :- !.
subst(P,V,E,Q) :-
	P =.. [Op|L],
	!,
	subst_list(L,V,E,L2),
	Q2 =.. [Op|L2],
        simplify(Q2,Q).
subst(A,_,_,A).

subst_list([],_,_,[]).
subst_list([H|T],V,E,[H2|T2]) :-
	subst(H,V,E,H2),
	subst_list(T,V,E,T2).


%____Code for pretty printing_______________

pprin((A and B), N) :-  !,
	simplify(A,A2), pprin(A2,N), write(' and '), nl,
	simplify(B,B2), pprin(B2,N).
pprin((A --> B), N) :-  !,
	simplify(A,A2), pprin(A2,N), nl,
	tabs(N), write('-->'), nl, M is N+1,
        simplify(B,B2), pprin(B2,M).
pprin(not(not(A)), N)  :-  !, simplify(A, A2), tabs(N), write(A2).
pprin(not(A), N)  :-  simplify(not, A, A2), tabs(N), write(A2).
pprin(not(A), N)  :-  tabs(N), write(' not '), write(A).
pprin(A, N) :-  tabs(N), write(A).

tabs(0) :- !.
tabs(N) :- write('   '), M is N-1, tabs(M).


%____Code for simplification _________________


simplify((1-1), 0) :- !.
simplify((1*1), 1) :- !.
simplify((X+0), X) :- !.
simplify((X+1)-1, X) :- !.
simplify((X-1)+1, X) :- !.
simplify((X+1)=<(Y+1), (X=<Y)) :- !.
simplify(1=<(Y+1), (0=<Y)) :- !.
simplify(X, X).

simplify(not, true, false) :- !.
simplify(not, false, true) :- !.
simplify(not,(I=<J), (I>=A)) :- simplify(J+1,A), !.
simplify(not,(I=<J-1),I>=J) :- !.
simplify(not,(X>Y), (X=<Y)) :- !.
simplify(not,(X<Y), (X>=Y)).



% ___________ LEXICAL ANALYZER  _____________________________________


lex(Stream,Tokens)  :-  get_chars(Stream,L), tokenize(L,Tokens), !.

get_chars(Str,L) :-  get_code(Str,C), get_chars(Str,C,L).

get_chars(_,46, [46]) :- !.				% 46 = period
get_chars(Str,C,  [C|L1]) :- get_chars(Str,L1).

tokenize([], []).
tokenize([C|L], L3)	:- white(C), skip_whites(L,L2), tokenize(L2,L3).
tokenize([C|L], [X|L3]) :- alpha(C), identifier(X,[C|L],L2), tokenize(L2,L3).
tokenize([C|L], [X|L3]) :- d09(C), digits(X,[C|L],L2), tokenize(L2,L3).
tokenize(L, [X|L3])     :- special(X,L,L2), tokenize(L2,L3).

skip_whites([], []).
skip_whites([C|L], L2) :- (white(C) -> skip_whites(L,L2); L2 = [C|L]).

white(9).  % tab
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

identifier(X) --> ident(L), {name(N,L), (keyword(N) -> X=N; X=id(N))}.

ident([X|L]) --> letter(X), legits(L).
ident([X])   --> letter(X).

legits([X|W]) --> legit(X), legits(W).
legits([X])   --> legit(X).

legit(X) --> letter(X) ; digit(X).

letter(X) --> [X],  {alpha(X)}.

alpha(X) :-  X > 64,  X < 91.
alpha(X) :-  X > 96,  X < 123.

keyword(true).
keyword(and).
keyword(or).
keyword(not).
keyword(if).
keyword(then).
keyword(else).
keyword(while).
keyword(program).
keyword(input).
keyword(output).
keyword(for).

digits(num(N)) --> digs(L), {name(N,L)}.

digs([X|L]) --> digit(X), digs(L).
digs([X]) --> digit(X).

digit(X) -->  [X],  {d09(X)}.

d09(X) :- X > 47,  X < 58.

