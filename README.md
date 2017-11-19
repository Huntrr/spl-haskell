# Much Ado About Monads
This is a (Shakespeare Programming Language)[http://shakespearelang.sourceforge.net/report/shakespeare/] interpreter and optimizer written in Haskell.


## Usage
TODO


## Authors
Ben Sandler
Hunter Lightman
Kasra Koushan


## Questions:
- Using libraries. From homework/other libraries (Parsec?)
- Instead of optimizations... x86 compilation?
    - Would be very inefficient (maybe not though since we can statically figure out which variables are accessed in which scenes!)
    - Or maybe just to C. Compilation of I/O to x86 just seems like a hassle.
- Profiling before/after optimization with QuickCheck to see if optimization makes programs strictly faster?? Or is that not right?
- Do our optimizations require a transformation to SSA? Because I'm not sure we could do that!

Questions about Evaluator
- Monad type for evaluator, same as from exercise?
- How to handle IO (evalSentence), does all of our code need to be in IO monad???
- How to annotate error as it propagates (i.e. add line and line number!)
- Idea for IO: "Execution Monad" that can "Block" on a state
    - So execute is `IO ()` and it runs this "Execution Monad" until it blocks
      for IO, sequences in necessary IO and then continues Execution based on
      the blocked state (maybe CPS style, Blocked execution gives a function
      that result of IO is fed into to continue execution...)


## Modules:
1. Parser (from library)
2. Language (AST stuff)
3. SPL Parser
4. Pretty Printer
5. Optimization
    - Arithmetic simplification
    - Constant propagation
    - Dead code elimination
    - Can we free up tails of stacks that are never used?
    - ...
6. Evaluator
7. Stepper
8. Vocabulary Generators
9. Tests
10. Compiler??? (maybe not.)
    - I think, ideally, we optimize the SPL AST and then compile to x86 or C. It'd be really neat to see how the optimized vs. unoptimized compiled code performed
    - But we might not have enough time
