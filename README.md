# quantity

Prolog package to parse quantities such as "1.5 kg" or "1E-10" or "p < .001". Its main
purpose is give feedback to users of an e-learning system to provide numeric input in
the correct format (e.g., t-ratios with 2 decimal places).

You can find a SWISH demo here, https://swish.swi-prolog.org/p/quantity-0.1.swinb

# Installation and usage

Simple usage example from the SWI-Prolog console:

```
pack_install(quantity).
use_module(library(quantity)).

quantity("1.5 kg", Number, Options).
```

For more examples, type "ex_quant."
