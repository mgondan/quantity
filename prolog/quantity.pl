:- module(quantity, [quantity/3]).
:- use_module(library(dcg/basics)).

%
% General numeric input like "1.5 cm"
%
quantity(String, Number, Options) :-
    string_codes(String, Codes), 
    quantity(Number, Options, Codes, []).

%
% 1.5E10
%
quantity(Number, Options) --> 
    number(Number, Options).

%
% 10 kg
%
quantity(Number, Options) --> 
    number(Num, NumOpt),
    blanks,
    unit(_Unit, UnitOpt),
    { option(fac(Kilo), UnitOpt, 1),
      Number is Num * Kilo, 
      option(dec(D), NumOpt, 0),
      Dec is D - log10(Kilo),
      merge_options([dec(Dec) | UnitOpt], NumOpt, Options)
    }.

% Real number
number(Number, Options) -->
    intdotfrac(Int, IntOpt),
    power(Pow),
    { Number is Int * 10^Pow, 
      option(dec(D), IntOpt, 0),
      Dec is D - Pow,
      merge_options([dec(Dec)], IntOpt, Options) 
    }.

% Sign
sign(+1, [sign(+)]) --> "+".
sign(-1, [sign(-)]) --> "-"; [226, 136, 146].
sign( 1, []) --> "".

% Natural number
nat(Number) -->
    digits([H | T]),
    { number_codes(Number, [H | T]) }.

int(Number, Options) -->
    sign(Sign, Options),
    nat(Num),
    { Number is Sign * Num }.

dot --> ".".
dot --> ",".

% Fractional part
frac(Number, [dec(Dec)]) --> 
    digits([H | T]),
    { number_codes(Num, [H | T]), 
      length([H | T], Dec), 
      Number is Num / 10.0^Dec
    }.

% 15
intdotfrac(Number, [dec(0) | Options]) -->
    int(Number, Options).
    
% 15.5
intdotfrac(Number, Options) -->
    int(Int, IntOpt),
    dot,
    frac(Frac, FracOpt),
    { Number is Int + Frac,
      merge_options(IntOpt, FracOpt, Options)
    }.

% .70
intdotfrac(Number, Options) -->
    sign(Sign, SgnOpt),
    dot,
    frac(Frac, FracOpt),
    { Number is Sign * Frac,
      merge_options(SgnOpt, [int(empty) | FracOpt], Options)
    }.
    
% times 10^E
power(Pow) --> 
    exp_e, 
    int(Pow, _).

power(0) --> [].

exp_e --> "E".
exp_e --> "e".
exp_e --> blanks, "*", blanks, "10", blanks, "^", blanks.
exp_e --> blanks, "*", blanks, "10", blanks, "**", blanks.
exp_e --> blanks, [195, 151], blanks, "10", blanks, "^", blanks.
exp_e --> blanks, [195, 151], blanks, "10", blanks, "**", blanks.

unit('%', [fac(0.01), si('%')]) --> "%".

unit(U, [si(SI) | Options]) --> 
    modifier(M, Options), 
    si(SI),
    { string_concat(M, SI, U) }.

% Modifier
modifier(k, [fac(1000)]) --> "k".
modifier(c, [fac(0.01)]) --> "c".
modifier(m, [fac(0.001)]) --> "m".
modifier('', [fac(1)]) --> "".

% Unit
si(g) --> "g".
si(m) --> "m".

ex_quant :-
    S = ".5 kg", 
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "1.5 kg",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "15 kg",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "15 g",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "-15 g",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "-15E10 g",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "-1.5E-10 g",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "0.09",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "9%",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).

ex_quant :-
    S = "9.1 %",
    quantity(S, N, Options),
    writeln(S), writeln(N), writeln(Options).
