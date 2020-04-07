:- module(quantity, [
    quantity/3, 
    match/3]).
    
:- use_module(library(dcg/basics)).

:- discontiguous qex/0.
:- discontiguous quant/3.
:- discontiguous from//1.
:- discontiguous to//1.
:- discontiguous places/3.

% Match two terms
match(Ref, Input, Diff) :-
    quantity(R, ROpt, Ref),
    quantity(I, IOpt, Input),
    match(R, ROpt, I, IOpt, Diff).

% Parse quantity
quantity(Term, Options, String) :-
    string(String),
    string_codes(String, Codes),
    quant(Term, Options, Codes).

quantity(Term, Options, Atom) :-
    atom(Atom),
    atom_codes(Atom, Codes),
    quant(Term, Options, Codes).

quantity(Term, Options, Codes) :-
    quant(Term, Options, Codes).

% Internal
match(Ref, ROpt, Input, IOpt, Diff) :-
    option(places(Default), IOpt, 2),
    option(places(Places), ROpt, Default),
    round(Ref * 10^Places) =:= round(Input * 10^Places),
    diff(ROpt, IOpt, Diff).

match(Ref, ROpt, Input, IOpt, Diff) :-
    option(places(Default), IOpt, 2),
    option(places(Places), ROpt, Default),
    Default =:= Places - 1,
    round(Ref * 10^Default) =:= round(Input * 10^Default),
    diff(ROpt, IOpt, Diff).

diff(Ref, Input, []) :-
    option(places(P), Ref),
    option(places(P), Input, P).

diff(Ref, Input, [["Please report the result with ", P, " decimal places."]]) :-
    option(places(P), Ref),
    option(places(Q), Input, P),
    P =\= Q.

% Term from codes
from(Term, Codes) :-
    format("~s", [Codes]),
    call(Term, Codes, [])
    -> format(" -> ~k~n", Term)
     ; format(": conversion failed~n").

qex(Codes) :-
    format("~s", [Codes]),
    quant(Term, Options, Codes),
    format(" -> ~k ~k~n", [Term, Options]).

% Term to codes
to(Term, Codes) :-
    format("~k", Term),
    call(Term, Codes, [])
    -> format(" -> ~s~n", [Codes])
     ; format(": conversion failed~n").

% Natural number
quant(Term, [type(natural)], Codes) :-
    natural(Term, Codes, []).

natural(N) -->
    { ground(N) }
    -> to(natural(N))
     ; from(natural(N)).

% Term to codes
to(natural(N)) -->
    { number_codes(N, Codes) },
    digits(Codes).

% Term from codes
from(natural(N)) -->
    digits([C | Codes]),
    { number_codes(N, [C | Codes]) }.

qex :- from(natural(_N), `123`).
qex :- qex(`123`).
qex :- to(natural(123), _Codes).

% Integer number
quant(Term, [type(integer) | Options], Codes) :-
    integ(Term, Options, Codes, []).

integ(I, Options) -->
    { ground(I) }
    -> to(integer(I, Options))
     ; from(integer(I, Options)).

to(integer(I, Options)) -->
    { S is integer(sign(I)),
      N is integer(abs(I))
    },
    sign(S, Options),
    natural(N).

from(integer(I, Options)) -->
    sign(S, Options),
    natural(N),
    { I is S * N }.

sign(S, Options) -->
    { ground(S) }
    -> to(sign(S, Options))
     ; from(sign(S, Options)).

to(sign(1, Options)) -->
    { option(sign(pretty), Options, default) },
    "".

to(sign(1, Options)) -->
    { option(sign(default), Options, default) },
    "+".

to(sign(0, _)) -->
    "".

to(sign(-1, Options)) -->
    { option(sign(pretty), Options, default) },
    [226, 136, 146].

to(sign(-1, Options)) -->
    { option(sign(default), Options, default) },
    "-".

from(sign(1, Options)) -->
    { memberchk(sign(pretty), Options) },
    "".

from(sign(1, Options)) -->
    { memberchk(sign(default), Options) },
    "+".

from(sign(-1, Options)) -->
    { memberchk(sign(pretty), Options) },
    [226, 136, 146].

from(sign(-1, Options)) -->
    { memberchk(sign(pretty), Options) },
    [8722].

from(sign(-1, Options)) -->
    { memberchk(sign(default), Options) },
    "-".

qex :- from(integ(_N, _Opt), `123`).
qex :- qex(`123`).
qex :- to(integ(123, [sign(pretty)]), _Codes).

qex :- from(integ(_N, _Opt), `-123`).
qex :- qex(`-123`).
qex :- to(integ(-123, [sign(pretty)]), _Codes).

% Real number
quant(Term, [type(real) | Options], Codes) :-
    real(Term, Options, Codes, []).

real(R, Options) -->
    { ground(R) }
    -> to(real(R, Options))
     ; from(real(R, Options)).

to(real(R, Options)) -->
    { I is float_integer_part(R),
      F is float_fractional_part(abs(R))
    },
    integ(I, Options),
    comma(R, Options),
    places(F, Options).

from(real(R, Options)) -->
    integ(I, Options),
    comma(_, Options),
    places(F, Options),
    { R is I + sign(I) * F }.

comma(R, Options) -->
    { ground(R) }
    -> to(comma(R, Options))
     ; from(comma(R, Options)).

to(comma(R, Options)) -->
    { option(unit(Unit), Options, none),
      places(R, Unit, Default),
      option(places(0), Options, Default)
    },
    "".

to(comma(R, Options)) -->
    { option(unit(Unit), Options, none),
      places(R, Unit, Default),
      option(places(Places), Options, Default),
      Places > 0,
      option(dec(.), Options, .)
    },
    ".".

to(comma(_, Options)) -->
    { option(places(Places), Options, 2),
      Places > 0,
      option(dec(,), Options)
    },
    ",".

from(comma(_, Options)) -->
    { memberchk(places(0), Options) },
    "".

from(comma(_, Options)) -->
    { memberchk(dec(.), Options) },
    ".".

from(comma(_, Options)) -->
    { memberchk(dec(,), Options) },
    ",".

places(F, Options) -->
    { ground(F) }
    -> to(places(F, Options))
     ; from(places(F, Options)).

to(places(F, Options)) -->
    { option(unit(Unit), Options, none),
      places(F, Unit, Default),
      option(places(0), Options, Default)
    },
    "".

to(places(F, Options)) -->
    { option(unit(Unit), Options, none),
      places(F, Unit, Default),
      option(places(Places), Options, Default),
      Places > 0,
      format(atom(Mask), '~~`0t~~d~~~w+', Places),
      format(codes(Codes), Mask, round(F * 10^Places))
    },
    digits(Codes).

from(places(0, Options)) -->
    { memberchk(places(0), Options) },
    "".

from(places(F, Options)) -->
    { memberchk(dec(_), Options) },
    digits([C | Codes]),
    { number_codes(N, [C | Codes]),
      length([C | Codes], Places),
      F is N / 10^Places,
      memberchk(places(Places), Options)
    }.

qex :- from(real(_N, _Opt), `12`).
qex :- qex(`12`).
qex :- to(real(12, []), _Codes).

qex :- from(real(_N, _Opt), `12.3`).
qex :- qex(`12.3`).
qex :- to(real(12.3, [places(1)]), _Codes).

qex :- from(real(_N, _Opt), `-12.3`).
qex :- qex(`-12.3`).
qex :- to(real(-12.3, []), _Codes).

%
% Number with unit
%
quant(Term, [type(measure) | Options], Codes) :-
    measure(Term, Options, Codes, []).

measure(R, Options) -->
    { ground(R) }
    -> to(measure(R, Options))
     ; from(measure(R, Options)).

to(measure(R, Options)) -->
    real(R, Options),
    unit(R, Options).

from(measure(R, Options)) -->
    real(R, Options),
    unit(R, Options).

unit(R, Options) -->
    { ground(R) }
    -> to(unit(R, Options))
     ; from(unit(R, Options)).

places(_, none, 2).
places(_, '%', 0).
places(_, m, 2).
places(_, kg, 1).

to(unit(_, Options)) -->
    { option(unit('%'), Options) },
    "%".

to(unit(_, Options)) -->
    { option(unit(m), Options) },
    " ",
    "m".

to(unit(_, Options)) -->
    { option(unit(kg), Options) },
    " ",
    "kg".

from(unit(_, Options)) -->
    blanks, "%",
    { memberchk(unit('%'), Options) }.

from(unit(_, Options)) -->
    blanks, "m",
    { memberchk(unit(m), Options) }.

from(unit(_, Options)) -->
    blanks, "kg",
    { memberchk(unit(kg), Options) }.

qex :- from(measure(_N, _Opt), `50%`).
qex :- qex(`50%`).
qex :- to(measure(50, [unit('%')]), _Codes).

qex :- from(measure(_N, _Opt), `12.3 kg`).
qex :- qex(`12.3 kg`).
qex :- to(measure(12.3, [unit(kg)]), _Codes).

%
% Statistic
%
quant(Term, [type(statistic) | Options], Codes) :-
    statistic(Term, Options, Codes, []).

statistic(S, Options) -->
    { ground(S) }
    -> to(statistic(S, Options))
     ; from(statistic(S, Options)).

to(statistic(S, Options)) -->
    { option(ratio(Unit), Options, none),
      places(S, Unit, Default),
      option(places(Places), Options, Default)
    },
    ratio(S, Options),
    " ",
    equals(S),
    " ",
    real(S, [sign(pretty), places(Places) | Options]).

from(statistic(S, Options)) -->
    ratio(S, Options),
    blanks,
    equals(S),
    blanks,
    real(S, Options).

equals(_) -->
    "=".

ratio(S, Options) -->
    { ground(S) }
    -> to(ratio(S, Options))
     ; from(ratio(S, Options)).

to(ratio(_, Options)) -->
    { option(ratio(z), Options)
    },
    "z".

from(ratio(_, Options)) -->
     "z",
     { memberchk(ratio(z), Options) }.

places(Z, z, 2) :-
    abs(Z) < 10.

places(Z, z, 1) :-
    abs(Z) >= 10.

to(ratio(_, Options)) -->
    { option(ratio(t(Df)), Options) },
    "t(", number(Df), ")".

from(ratio(_, Options)) -->
    "t(", number(Df), ")",
     { memberchk(ratio(t(Df)), Options) }.

places(T, t(_), 2) :-
    abs(T) < 10.

places(T, t(_), 1) :-
    abs(T) >= 10.

to(ratio(_, Options)) -->
    { option(ratio('F'(Df1, Df2)), Options) },
    "F(", number(Df1), ", ", number(Df2), ")".

from(ratio(_, Options)) -->
    "F(", number(Df1), ",", blanks, number(Df2), ")",
     { memberchk(ratio('F'(Df1, Df2)), Options) }.

places(F, 'F'(_, _), 2) :-
    abs(F) < 10.

places(F, 'F'(_, _), 1) :-
    abs(F) >= 10.

qex :- from(statistic(_N, _Opt), `z = 1.96`).
qex :- qex(`z = 1.96`).
qex :- to(statistic(1.9, [ratio(z)]), _Codes).
qex :- to(statistic(19.61, [ratio(z)]), _Codes).

qex :- from(statistic(_N, _Opt), `t(15) = 2.12`).
qex :- qex(`t(15) = 2.12`).
qex :- to(statistic(2.1, [ratio(t(15))]), _Codes).
qex :- to(statistic(21.21, [ratio(t(15))]), _Codes).

qex :- from(statistic(_N, _Opt), `F(10, 150) = 12.12`).
qex :- qex(`F(10, 150) = 12.12`).
qex :- to(statistic(2.1, [ratio('F'(10, 150))]), _Codes).
qex :- to(statistic(12.12, [ratio('F'(10, 150))]), _Codes).
