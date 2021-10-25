:- module(quantity, [ quantity/3 ]).

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

:- use_module(library(dcg/basics)).

sign(+1, [sign(none)]) -->
    "".

sign(+1, [sign(plus)]) -->
    "+".

sign(-1, [sign(hyphen)]) -->
    "-".

sign(-1, [sign(dash)]) -->
    [226, 136, 146].

sign(-1, [sign(minus)]) -->
    [8722].

nat(N) -->
    digits([H | Codes]),
    { number_codes(N, [H | Codes]) }.

int(I, Options) -->
    sign(S, Options),
    nat(N),
    { I is S * N }.

sep(dot) -->
    ".".

sep(comma) -->
    ",".

frac(F, [frac(given), sep(S), digits(D)]) -->
    sep(S),
    digits([H | Codes]),
    { number_codes(N, [H | Codes]),
      length(Codes, L),
      D is L + 1,
      F = N / 10^D
    }.

% 1.23
real(R, [int(given) | Options]) -->
    sign(S, Opt1),
    nat(N),
    frac(F, Opt2),
    { R is S * (N + F),
      append([Opt1, Opt2], Options)
    }.

% .77
real(R, [int(none) | Options]) -->
    sign(S, Opt1),
    frac(F, Opt2),
    { R is S * F,
      append([Opt1, Opt2], Options)
    }.

% 12
real(R, [int(given), frac(none) | Options]) -->
    sign(S, Options),
    nat(N),
    { R is S * N }.

quantity(Q, [type(integer) | Options]) -->
    int(Q, Options).

quantity(Q, [type(real) | Options]) -->
    real(Q, Options).
