:- module(quantity, [ quantity/0, quantity/3 ]).
:- use_module(library(dcg/basics)).

:- set_prolog_flag(float_overflow, infinity).
:- set_prolog_flag(float_undefined, nan).
:- set_prolog_flag(float_zero_div, infinity).

% Parse quantity
quantity(Number, Options, String) :-
    string(String),
    string_codes(String, Codes),
    quantity(Number, Options, Codes).

quantity(Number, Options, Atom) :-
    atom(Atom),
    atom_codes(Atom, Codes),
    quantity(Number, Options, Codes).

quantity(Number, Options, [H | Codes]) :-
    quantity(Number, Options, [H | Codes], []).

quantity(Number, Options, [H | Codes]) :-
    interval(Number, Options, [H | Codes], []).

% Examples
quantity :-
    quantity("2.5").

quantity :-
    quantity("+2.5").

quantity :-
    quantity("-2.5").

quantity :-
    quantity(".5").

quantity :-
    quantity("-.5").

quantity :-
    quantity("-5").

quantity :-
    quantity("5").

quantity :-
    quantity("-3.3 to -3.2").

quantity(String) :-
    quantity(N, Options, String),
    writeln(string(String)-number(N)-options(Options)).

% Main types
:- discontiguous quantity//2.

quantity(Q, [type(natural)])
--> nat(Q).

quantity(Q, [type(integer) | Options])
--> int(Q, Options).

quantity(Q, [type(real) | Options])
--> real(Q, Options).

interval(ci(Lo, Hi), Options)
--> quantity(Lo, LoOpt),
    to(ToOpt),
    quantity(Hi, HiOpt),
    { append([LoOpt, ToOpt, HiOpt], Options) }.

% Components
sign(+1, [sign(none)])
--> "".

sign(+1, [sign(plus)])
--> "+".

sign(-1, [sign(hyphen)])
--> "-".

sign(-1, [sign(dash)])
--> [226, 136, 146].

sign(-1, [sign(minus)])
--> [8722].

nat(N)
--> digits([H | Codes]),
    { number_codes(N, [H | Codes]) }.

int(I, Options)
--> sign(S, Options),
    nat(N),
    { I is S * N }.

sep(dot)
--> ".".

sep(comma)
--> ",".

frac(F, [frac(given), sep(S), digits(D)])
--> sep(S),
    digits([H | Codes]),
    { number_codes(N, [H | Codes]),
      length(Codes, L),
      D is L + 1,
      F is N / 10^D
    }.

real(R, [int(given) | Options])
--> sign(S, Opt1),
    nat(1),
    frac(0, Opt2),
    "Inf",
    { R is S * 1.0Inf,
      append([Opt1, Opt2], Options)
    }.

% 1.23
real(R, [int(given) | Options])
--> sign(S, Opt1),
    nat(N),
    frac(F, Opt2),
    { R is S * (N + F),
      append([Opt1, Opt2], Options)
    }.

% .77
real(R, [int(none) | Options])
--> sign(S, Opt1),
    frac(F, Opt2),
    { R is S * F,
      append([Opt1, Opt2], Options)
    }.

% 12
real(R, [int(given), frac(none) | Options])
--> sign(S, Options),
    nat(N),
    { R is S * N }.

% Intervals (e.g., confidence intervals)
to([to(to)])
--> blank, blanks, "to", blank, blanks.

to([to(dotdotdot)])
--> blank, blanks, "...", blank, blanks.

to([to(dash)])
--> blank, blanks, [226, 136, 146], blank, blanks.

to([to(hyphen)])
--> blank, blanks, "-", blank, blanks.
