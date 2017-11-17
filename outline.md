## Questions:
- Using libraries. From homework/other libraries (Parsec?)
- Instead of optimizations... x86 compilation?
    - Would be very inefficient (maybe not though since we can statically figure out which variables are accessed in which scenes!)
- Do our optimizations require a transformation to SSA? Because I'm not sure we could do that!

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
6. Interpreter
7. Stepper
8. Vocabulary Generators
9. Tests
