# SymbolicHaskell

A simple Haskell CAS DSL.

Currently only simple simplification rules are implemented, however it is very
easy to add new rules. More rules are needed for properly reducing expressions.
A parser along with a rule description language is also planned.

# Example

```haskell
import Math.Symbolic.Simplify

main = do
    print $. simplify $ x^2 + 5 + x^2 + x*x
    -- (3.0 * (x ^ 2.0)) + 5.0
```
