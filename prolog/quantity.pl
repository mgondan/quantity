:- module(quantity, [ quantity/3, match/3 ]).

:- use_module(library(dcg/basics)).

% Produce or parse quantity
quantity(Term, Options, String) :-
    string(String),
    string_codes(String, Codes),
    quantity(Term, Options, Codes).

quantity(Term, Options, Atom) :-
    atom(Atom),
    atom_codes(Atom, Codes),
    quantity(Term, Options, Codes).

quantity(Term, Options, Codes) :-
    ground(Codes),
    quant(Term, Options, Codes, []).

quantity(Term, Options, Codes) :-
    ground(Term),
    fmt(Term, Options, Codes, []).

% Match two quantities
match(Ref, Input, Diff) :-
    quantity(R, ROpt, Ref),
    quantity(I, IOpt, Input),
    match(R, ROpt, I, IOpt, Diff).

% Internal
match(Ref, ROpt, Input, IOpt, Diff) :-
    option(dec(Default), IOpt, 2),
    option(dec(Places), ROpt, Default),
    arg(1, Ref, R),
    arg(1, Input, I),
    round(R * 10^Places) =:= round(I * 10^Places),
    diff(ROpt, IOpt, Diff).

match(Ref, ROpt, Input, IOpt, Diff) :-
    option(dec(Default), IOpt, 2),
    option(dec(Places), ROpt, Default),
    Default =:= Places - 1,
    arg(1, Ref, R),
    arg(1, Input, I),
    round(R * 10^Default) =:= round(I * 10^Default),
    diff(ROpt, IOpt, Diff).

diff(Ref, Input, []) :-
    option(dec(P), Ref),
    option(dec(P), Input, P).

diff(Ref, Input, [["Please report the result with ", P, " decimal places."]]) :-
    option(dec(P), Ref),
    option(dec(Q), Input, P),
    P =\= Q.

% Term to codes
fmt(natural(N), Options) -->
    fmt(nat(N), Options).

fmt(integer(I), Options) -->
    { S is round(sign(I)),
      N is round(abs(I))
    },
    fmt(sgn(S), Options),
    fmt(nat(N), Options).

fmt(float(R), Options) -->
    { option(dec(0), Options, 2),
      I is float_integer_part(R)
    },
    fmt(integer(I), Options).

fmt(float(R), Options) -->
    { option(dec(Places), Options, 2),
      Places > 0,
      S is round(sign(R)),
      N is round(abs(float_integer_part(R))),
      F is abs(float_fractional_part(R))
    },
    fmt(sgn(S), Options),
    fmt(nat(N), Options),
    fmt(dot, Options),
    fmt(dec(F), Options).

fmt(amount(R), Options) -->
    fmt(float(R), Options),
    " ",
    fmt(si, Options).

fmt(statistic(S), Options) -->
    fmt(ratio, Options),
    " ",
    fmt(equals, Options),
    " ",
    fmt(float(S), Options).

% Helpers
fmt(nat(N), _) -->
    { number_codes(N, Codes) },
    digits(Codes).

fmt(sgn(+1), Options) -->
    { option(sign(none), Options, none) },
    "".

fmt(sgn(+1), Options) -->
    { option(sign(plus), Options) },
    "+".

fmt(sgn(0), _) -->
    "".

fmt(sgn(-1), Options) -->
    { option(sign(hyphen), Options) },
    "-".

fmt(sgn(-1), Options) -->
    { option(sign(dash), Options) },
    [226, 136, 146].

fmt(sgn(-1), Options) -->
    { option(sign(minus), Options) },
    [8722].

fmt(dot, Options) -->
    { option(dec(0), Options, 2) },
    "".

fmt(dot, Options) -->
    { option(dec(Places), Options, 2),
      Places > 0,
      option(dot(.), Options, (.))
    },
    ".".

fmt(dot, Options) -->
    { option(dec(Places), Options, 2),
      Places > 0,
      option(dot(,), Options)
    },
    ",".

fmt(dec(F), Options) -->
    { option(dec(Places), Options, 2),
      Places > 0,
      format(atom(Mask), '~~`0t~~d~~~w+', Places),
      format(codes(Codes), Mask, round(F * 10^Places))
    },
    digits(Codes).

fmt(si, Options) -->
    { option(unit(kg), Options) },
    "kg".

fmt(si, Options) -->
    { option(unit(m), Options) },
    "m".

fmt(si, Options) -->
    { option(unit(s), Options) },
    "s".

fmt(equals, Options) -->
    { option(equals(=), Options, =) },
    "=".

fmt(equals, Options) -->
    { option(equals(<), Options) },
    "<".

fmt(equals, Options) -->
    { option(equals(>), Options) },
    ">".

fmt(ratio, Options) -->
    { option(ratio(z), Options) },
    "z".

fmt(ratio, Options) -->
    { option(ratio(t), Options),
      option(df(Df), Options)
    },
    "t(", fmt(df(Df), Options), ")".

fmt(ratio, Options) -->
    { option(ratio('F'), Options),
      option(df1(Df1), Options),
      option(df2(Df2), Options),
      option(dot(.), Options, '.')
    },
    "F(", fmt(df(Df1), Options), ", ", fmt(df(Df2), Options), ")".

fmt(ratio, Options) -->
    { option(ratio('F'), Options),
      option(df1(Df1), Options),
      option(df2(Df2), Options),
      option(dot(,), Options)
    },
    "F(", fmt(df(Df1), Options), "; ", fmt(df(Df2), Options), ")".

fmt(df(Df), Options) -->
    { integer(Df) },
    fmt(nat(Df), Options).

fmt(df(Df), Options) -->
    { float(Df),
      select_option(dec(_), Options, New)
    },
    fmt(float(Df), [dec(1) | New]).

% Term from codes
quant(natural(N), Options) -->
    nat(N, Options).

quant(integer(I), Options) -->
    int(I, Options).

quant(float(F), Options) -->
    flt(F, Options).

quant(amount(F), Options) -->
    amt(F, Options).

quant(statistic(S), Options) -->
    stat(S, Options).

nat(N, []) -->
    digits([C | Codes]),
    { number_codes(N, [C | Codes]) }.

int(I, Options) -->
    sgn(S, Opt1),
    nat(N, Opt2),
    { I is S * N,
      append(Opt1, Opt2, Options)
    }.

flt(R, Options) -->
    sgn(S, Opt1),
    nat(N, Opt2),
    dec(F, Opt3),
    { R is S * (N + F),
      append([Opt1, Opt2, Opt3], Options)
    }.

amt(F, Options) -->
    flt(F, Opt1),
    blanks,
    si(Opt2),
    { append(Opt1, Opt2, Options) }.

stat(S, Options) -->
    ratio(Opt1),
    blanks,
    equals(Opt2),
    blanks,
    flt(S, Opt3),
    { append([Opt1, Opt2, Opt3], Options) }.

sgn(+1, [sign(none)]) -->
    "".

sgn(+1, [sign(plus)]) -->
    "+".

sgn(-1, [sign(hyphen)]) -->
    "-".

sgn(-1, [sign(dash)]) -->
    [226, 136, 146].

sgn(-1, [sign(minus)]) -->
    [8722].

dot([dot(none)]) -->
    "".

dot([dot(.)]) -->
    ".".

dot([dot(,)]) -->
    ",".

dot([dot(;)]) -->
    ";".

dec(0, [dec(0) | Options]) -->
    dot(Options),
    { member(dot(none), Options) },
    "".

dec(D, [dec(Places) | Options]) -->
    dot(Options),
    { member(dot(Dot), Options),
      dif(Dot, none)
    },
    digits([C | Codes]),
    { number_codes(N, [C | Codes]),
      length([C | Codes], Places),
      D is N / 10^Places
    }.

si([unit(kg)]) -->
    "kg".

si([unit(m)]) -->
    "m".

si([unit(s)]) -->
    "s".

ratio([ratio(z)]) -->
    "z".

ratio([ratio(t), df(Df)]) -->
    "t(", flt(Df, _), {Df > 0}, ")".

ratio([ratio(t), df(Df)]) -->
    "t_", flt(Df, _), {Df > 0}.

ratio([ratio('F'), df1(Df1), df2(Df2)]) -->
    "F(", flt(Df1, Opt1),
    dot(Opt2),
    blanks,
    flt(Df2, Opt3), ")",
    { ( member(dot(none), Opt1); member(dot(.), Opt1) ),
      ( member(dot(,), Opt2); member(dot(;), Opt2) ),
      ( member(dot(none), Opt3); member(dot(.), Opt3) ),
      Df1 > 0,
      Df2 > 0
    }.

ratio([ratio('F'), df1(Df1), df2(Df2)]) -->
    "F(", flt(Df1, Opt1),
    dot(Opt2),
    blanks,
    flt(Df2, Opt3), ")",
    { member(dot(,), Opt1),
      member(dot(;), Opt2),
      member(dot(,), Opt3),
      Df1 > 0,
      Df2 > 0
    }.

equals([equals(=)]) -->
    "=".

equals([equals(<)]) -->
    "<".

equals([equals(>)]) -->
    ">".

example([C | Codes]) :-
    format("~s", [[C | Codes]]),
    quantity(Q, Options, [C | Codes]),
    format(" -> ~k ~w", [Q, Options]),
    quantity(Q, Options, Back),
    format(" --> ~s~n", [Back]).

:- discontiguous example/0.

example :- example(`123`).
example :- example(`-123`).
example :- example(`0`).
example :- example(`-123.456`).
example :- example(`-0.5`).
example :- example(`-0.05`).
example :- example(`10 kg`).
example :- example(`z = -1.96`).
example :- example(`t(22) = 2.23`).
example :- example(`t(22.1) = 2.23`).
example :- example(`F(1, 12) = 2.23`).

example(Ref, Input) :-
    format("~s ~s", [Ref, Input]),
    match(Ref, Input, Diff),
    !,
    format(" -> ~w~n", [Diff]).

example(Ref, Input) :-
    format("~s ~s (no match)~n", [Ref, Input]).

example :- example(`12.1`, `12.12`).
example :- example(`12.12`, `12.1`).
example :- example(`12.1`, `12`).

