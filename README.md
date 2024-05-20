## Vau lisp?
[Fexprs as the basis of Lisp function application or $vau : the ultimate abstraction](https://web.cs.wpi.edu/~jshutt/dissertation/etd-090110-124904-Shutt-Dissertation.pdf) by John Shutt

The core idea I implement here takes inspiration from this dissertation. $vau is a reworking of f-exprs (essentially, first-class macros) on which all other forms can be implemented. It reframes lisp evaluation from implicit evaluation with explicit quoting to one of implicit quoting with explicit evaluation.

Effectively, applying a vau expression acts as a tree rewrite rule. Evaluation occurs as a result of the `wrap` primitive, which converts a combiner into an applicative, evaluating its arguments before applying the combiner.

The primitives of the language are:
- $def! - definition of one name to one value
- eq? - identity comparison of just two values
- $vau - binds parameters to a single name, unpacking is manual
- wrap - wrap a combiner as an applicative
- unwrap - remove an applicative wrapping
- car/cdr - operators for decomposing pairs
- make-environment - create a new environment
- eval - evaluate an expression in an environment
- select - if a value is true, return the first argument, else the second

## Goals
- [x] Core language and REPL in Python
- [ ] Meta-circular evaluator
- [ ] Self-compile to wasm WAT format