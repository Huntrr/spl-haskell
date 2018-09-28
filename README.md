# Much Ado About Monads
This is a [Shakespeare Programming Language](http://shakespearelang.sourceforge.net/report/shakespeare/) interpreter and optimizer written in Haskell.

## Usage
Build with `ghc Main.hs -o spl` and then run with `spl filename [timeout]` which
will execute `filename` for `timeout` steps (or forever if no `timeout` is
specified.

# Packages
- megaparsec
- roman-numerals


## Authors
Ben Sandler - sandlerb
Hunter Lightman - hunterl
Kasra Koushan koushan


## Modules (in approximate order for you to read it in):
1. AST.hs -- high level description of our AST for SPL
1. WordLists.hs -- Simple, python-generated file containing the different wordlists, needed by the Parer
1. LanguageParser.hs -- The parser itself
1. Evaluator.hs -- The evaluator for SPL, core of evaluation logic is all here
1. Stepper.hs -- A couple auxiliary functions using the evaluator to provide the ability to execute a set number of steps of the program.
1. ExceptionPrinter -- Optional. Utilities for pretty printing exceptions for better error handling/debugging.
1. Optimize.hs -- A couple of optimization oriented function for the AST. Mainly concerned with arithmetic simplification of expressions.
1. PrettyPrinter.hs -- The SPL pretty printer
1. Tests.hs -- Tests for all of our modules
1. Main.hs -- The glue that ties it all together for our executable.
