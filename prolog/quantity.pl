:- module(quantity, [ quantity/3, match/3, quantity_mathml/3, quantity_paren/3, quantity_prec/3, op(150, xfx, ...) ]).

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

% match interval
match_(R, I, Places) :-
    number(R),
    round(R * 10^Places) =:= round(I * 10^Places).
    
match_(L ... U, I, Places) :-
    round(L * 10^Places) =< round(I * 10^Places),
    round(U * 10^Places) >= round(I * 10^Places).
    
% Internal
match(Ref, ROpt, Input, IOpt, Diff) :-
    option(dec(Default), IOpt, 2),
    option(dec(Places), ROpt, Default),
    Ref =.. [_ | RArgs],
    Input =.. [_ | IArgs],
    maplist({Places}/[R, I] >> match_(R, I, Places), RArgs, IArgs),
    diff(ROpt, IOpt, Diff).

match(Ref, ROpt, Input, IOpt, Diff) :-
    option(dec(Default), IOpt, 2),
    option(dec(Places), ROpt, Default),
    Default =:= Places - 1,
    Ref =.. [_ | RArgs],
    Input =.. [_ | IArgs],
    maplist({Default}/[R, I] >> match_(R, I, Default), RArgs, IArgs),
    diff(ROpt, IOpt, Diff).

diff(Ref, Input, []) :-
    option(dec(P), Ref),
    option(dec(P), Input, P).

diff(Ref, Input, [["Please report the result with ", P, " decimal places."]]) :-
    option(dec(P), Ref),
    option(dec(Q), Input, P),
    P =\= Q.

% Convert to mathml
quantity_mathml(Q, Options, M) :-
    qmathml(Q, Options, M).

quantity_paren(Q, Options, P) :-
    qparen(Q, Options, P).

quantity_prec(Q, Options, P) :-
    qprec(Q, Options, P).

% Mathml rendering
:- discontiguous qmathml/3.
qmathml(natural(Q), _Options, mn(Q)).

:- discontiguous qparen/3.
qparen(natural(_), _Options, 0).    

:- discontiguous qprec/3.
qprec(natural(_), _Options, num-0).

qmathml(integer(I), _Options, mn(I)) :-
    I >= 0.
    
qmathml(integer(I), Options, mrow([mo(-), M])) :-
    I < 0,
    A is abs(I),
    qmathml(integer(A), Options, M).
    
qparen(integer(_), _Options, 0).

qprec(integer(I), _Options, num-0) :-
    I >= 0.
    
qprec(integer(I), _Options, op-(Prec)) :-
    I < 0,
    current_op(Prec, yfx, -).
    
qmathml(float(L ... U), Options, mrow([Lower, mo(&(hellip)), Upper])) :-
    qmathml(float(L), Options, Lower),
    qmathml(float(U), Options, Upper).

qmathml(float(R), Options, M) :-
    number(R),
    option(mod('%'), Options),
    qmathml(perc(R), Options, M).
    
qmathml(float(F), Options, mn(S)) :-
    number(F),
    F >= 0,
    option(dec(D), Options, 2),
    format(atom(Mask), "~~~df", [D]),
    format(string(S), Mask, [F]).

qmathml(float(F), Options, mn(S)) :-
    number(F),
    F >= 0,
    option(dec(D), Options, 2),
    format(atom(Mask), "~~~df", [D]),
    format(string(S), Mask, [F]).
    
qmathml(float(F), Options, mrow([mo(-), M])) :-
    number(F),
    F < 0,
    A is abs(F),
    qmathml(float(A), Options, M).
   
qparen(float(_), _Options, 0).

qprec(float(L ... _), Options, Prec) :-
    qprec(L, Options, Prec).
    
qprec(float(F), _Options, num-0) :-
    number(F),
    F >= 0.

qprec(float(F), _Options, op-(Prec)) :-
    number(F),
    F < 0,
    current_op(Prec, yfx, -).
    
qmathml(prob(F), Options, mn(S)) :-
    option(dec(D), Options, 3),
    format(atom(Mask), "~~~df", [D]),
    format(string(S), Mask, [F]).
    
qparen(prob(_), _Options, 0).

qprec(prob(_), _Options, num-0).

qmathml(perc(F), Options, mrow([mn(S), mo('%')])) :-
    option(dec(D), Options, 3),
    DP is max(0, D - 2),
    format(atom(Mask), "~~~df", [DP]),
    format(string(S), Mask, [100*F]).
    
qparen(perc(_), _Options, 0).

qprec(perc(_), _Options, num-0).

qmathml(amount(S), Options, mrow([F, &(nbsp), mtext(U)])) :-
    qmathml(float(S), Options, F),
    option(unit(U), Options).
    
qparen(amount(_), _Options, 0).

qprec(amount(_), _Options, op-Prec) :-
    current_op(P, yfx, *),
    Prec is P-1.
    
qmathml(statistic(S), Options, mrow([mi(R), mo(Op), F])) :-
    option(df(none), Options, none),
    option(ratio(R), Options),
    option(equals(Op), Options, =),
    qmathml(float(S), Options, F).
    
qparen(statistic(_), Options, 0) :-
    option(df(none), Options, none).
    
qprec(statistic(_), Options, op-Prec) :-
    option(equals(Op), Options, =),
    current_op(Prec, xfx, Op).
    
qmathml(statistic(S), Options, mrow([msubsup([mi('X'), mrow([mtext(df), mo(=), mn(Df)]), mn(2)]), mo(Op), F])) :-
    option(df(Df), Options),
    option(ratio(chi2), Options),
    option(equals(Op), Options, =),
    qmathml(float(S), Options, F).
    
qmathml(statistic(S), Options, mrow([msub([mi(R), mrow([mtext(df), mo(=), mn(Df)])]), mo(Op), F])) :-
    option(df(Df), Options),
    option(ratio(R), Options),
    option(equals(Op), Options, =),
    qmathml(float(S), Options, F).
    
qparen(statistic(_), Options, 1) :-
    option(df(_), Options).

qmathml(statistic(S), Options, mrow([msub([mi(R), mrow([mtext(df), mo(=), mn(Df1), mo(','), mn(Df2)])]), mo(Op), F])) :-
    option(df1(Df1), Options),
    option(df2(Df2), Options),
    option(ratio(R), Options),
    option(equals(Op), Options, =),
    qmathml(float(S), Options, F).
    
qparen(statistic(_), Options, 1) :-
    option(df1(_), Options),
    option(df2(_), Options).

qparen(confint(Lo, Up), Options, P) :-
    qparen(float(Lo), Options, P1),
    qparen(float(Up), Options, P2),
    P is max(P1, P2).
    
qprec(confint(_, _), Options, op-Prec) :-
    option(equals(Op), Options, =),
    current_op(Prec, xfx, Op).
    
qmathml(confint(Lo, Up), Options, mrow([Lower, &(nbsp), mtext(to), &(nbsp), Upper])) :-
    qmathml(float(Lo), Options, Lower),
    qmathml(float(Up), Options, Upper).
    
qparen(confint(Lo, Up), Options, P) :-
    qparen(float(Lo), Options, P1),
    qparen(float(Up), Options, P2),
    P is max(P1, P2).
    
qprec(confint(_, _), Options, op-Prec) :-
    option(equals(Op), Options, =),
    current_op(Prec, xfx, Op).
    
% Term to codes
fmt(natural(N), Options) -->
    fmt(nat(N), Options).

fmt(integer(I), Options) -->
    { S is round(sign(I)),
      N is round(abs(I))
    },
    fmt(sgn(S), Options),
    fmt(nat(N), Options).

fmt(float(Lower ... Upper), Options) -->
    fmt(float(Lower), Options), 
    " ... ",
    fmt(float(Upper), Options).

fmt(float(R), Options) -->
    { number(R),
      select_option(mod('%'), Options, New),
      R100 is 100*R,
      select_option(dec(Places), New, New2),
      P is Places-2
    },
    fmt(float(R100), [dec(P) | New2]), "%".
    
fmt(float(R), Options) -->
    { number(R),
      option(dec(0), Options, 2),
      I is float_integer_part(R)
    },
    fmt(integer(I), Options).

fmt(float(R), Options) -->
    { number(R),
      option(dec(Places), Options, 2),
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

fmt(confint(Lo, Up), Options) -->
    fmt(float(Lo), Options), 
    " to ",
    fmt(float(Up), Options).

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
    { option(ratio(chi2), Options),
      option(df(Df), Options)
    },
    "X²(", fmt(df(Df), Options), ")".

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

quant(confint(Lo, Up), Options) -->
    inter(Lo, Up, Options).

nat(N, []) -->
    digits([C | Codes]),
    { number_codes(N, [C | Codes]) }.

int(I, Options) -->
    sgn(S, Opt1),
    nat(N, Opt2),
    { I is S * N,
      append(Opt1, Opt2, Options)
    }.

flt(Lower ... Upper, Options) -->
    fl_(Lower, Opt1),
    blanks,
    "...",
    blanks,
    fl_(Upper, Opt2),
    { append(Opt1, Opt2, Options) }.

flt(R, Options) -->
    fl_(R, Options).
    
fl_(R, Options) -->
    sgn(S, Opt1),
    nat(N, Opt2),
    dec(F, Opt3),
    blanks,
    perc,
    { 
      select_option(dec(Places), Opt3, Opt4),
      P is Places+2,
      R is S * (N + F) / 100,
      append([Opt1, Opt2, [dec(P), mod('%')], Opt4], Options)
    }.

fl_(R, Options) -->
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

inter(Lo, Up, Options) -->
    flt(Lo, Opt1),
    blanks,
    "to",
    blanks,
    flt(Up, Opt2),
    { append(Opt1, Opt2, Options) }.

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

perc -->
    "%".

si([unit(kg)]) -->
    "kg".

si([unit(m)]) -->
    "m".

si([unit(s)]) -->
    "s".

ratio([ratio(z)]) -->
    "z".

ratio([ratio(t), df(Df)]) -->
    "t", applyfunction, "(", flt(Df, _), {Df > 0}, ")".

ratio([ratio(t), df(Df)]) -->
    "t_", flt(Df, _), {Df > 0}.

ratio([ratio(chi2), df(Df)]) -->
    "chi2", applyfunction, "(", flt(Df, _), {Df > 0}, ")".

ratio([ratio(chi2), df(Df)]) -->
    "chi2_", flt(Df, _), {Df > 0}.

ratio([ratio(chi2), df(Df)]) -->
    "X²", applyfunction, "(", flt(Df, _), {Df > 0}, ")".

ratio([ratio(chi2), df(Df)]) -->
    "X²_", flt(Df, _), {Df > 0}.

ratio([ratio(chi2), df(Df)]) -->
    "X2", applyfunction, "(", flt(Df, _), {Df > 0}, ")".

ratio([ratio(chi2), df(Df)]) -->
    "X2_", flt(Df, _), {Df > 0}.

ratio([ratio('F'), df1(Df1), df2(Df2)]) -->
    "F", applyfunction, "(", flt(Df1, Opt1),
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

applyfunction -->
    "".

applyfunction -->
    "%E2%81%A1".
    
applyfunction -->
    "\u2061".
    
applyfunction -->
    [8289].

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
example :- example(`0.1 ... 0.2`).
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

