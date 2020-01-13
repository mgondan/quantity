:- module(quantity, [
    quantity/3, 
    match_quantities/3]).
    
:- use_module(library(dcg/basics)).

%
% General numeric input like "1.5 cm"
%
quantity(String, Number, Options) :-
    ground(String),
    !, string_codes(String, Codes), 
    quantity(Number, Options, Codes, []).

quantity(String, Number, Options) :-
    ground(Number), 
    !, ( option(dec(D), Options) -> format(atom(Mask), '~~~wf', D) ; Mask = '~g' ),
    format(string(String), Mask, Number).

match_quantities(Solution, Response, []) :-
    quantity(Solution, Number_sol, Options_sol),
    option(dec(Dec), Options_sol),
    quantity(Response, Number_res, Options_res),
    option(dec(Dec), Options_res),
    Number_sol = Number_res.
    
% match_quantities(Solution, Response, [buggy(dec(Response) \= Sol, FB)]) :-
match_quantities(Solution, Response, [FB]) :-
    quantity(Solution, Number_sol, Options_sol),
    option(dec(Sol), Options_sol),
    quantity(Response, Number_res, Options_res),
    option(dec(Res), Options_res),
    Sol \= Res,
    N is round(Number_sol * 10^min(Sol, Res)) / 10^min(Sol, Res),
    N is round(Number_res * 10^min(Sol, Res)) / 10^min(Sol, Res),
    FB = ["Please report the result with ", Sol, " decimal place(s)."].
    
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

% sign(?Number, ?Options, ?Codes)//
sign(+1, [sign(+)]) --> 
    "+".

sign(-1, [sign(-)]) --> 
    "-".

sign(-1, [sign(-)]) --> 
    [226, 136, 146].

sign(1, [sign(none)]) --> 
    [].

% Natural number nat(?Number)//
nat(Number) -->
    { ground(Number),
      number_codes(Number, Dig)
    }, !, digits(Dig).

nat(Number) -->
    digits([H | T]),
    { number_codes(Number, [H | T]) }.

% Decimal separator dot//
dot --> ".".
dot --> ",".

% Fractional part frac(?Num, ?Options)//
frac(Number, [dec(Dec)]) -->
    { ground(Number),
      Num is round(Number * 10.0^Dec),
      number_codes(Num, Dig)
    }, !, digits(Dig).

frac(Number, [dec(Dec)]) -->
    digits([H | T]),
    { number_codes(Num, [H | T]),
      length([H | T], Dec),
      Number is Num / 10.0^Dec
    }.

% 15
intdotfrac(Number, [dec(0) | Options]) -->
    sign(Sign, Options),
    nat(Int),
    { Number is Sign * Int }.
    
% 15.5
intdotfrac(Number, Options) -->
    sign(Sign, SgnOpt),
    nat(Int),
    dot,
    frac(Frac, FracOpt),
    { Number is Sign * (Int + Frac),
      merge_options(SgnOpt, FracOpt, Options)
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
    sign(Sign, _),
    nat(P),
    { Pow is Sign * P }.
    
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

ex_quant :-
    Sol = "3.14", Res = "3.141",
    match_quantities(Sol, Res, Comment),
    writeln(Sol), writeln(Res), writeln(Comment).

ex_quant :-
    Sol = "3.141", Res = "3.14",
    match_quantities(Sol, Res, Comment),
    writeln(Sol), writeln(Res), writeln(Comment).

ex_quant :-
    Sol = "3.14", Res = "3.14",
    match_quantities(Sol, Res, Comment),
    writeln(Sol), writeln(Res), writeln(Comment).
