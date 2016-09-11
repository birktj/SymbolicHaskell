# SymbolicHaskell

A simple Haskell CAS DSL.

Currently only simple simplification rules are implemented. More rules are
needed for properly reducing expressions. Rules for factoring and
derivation are in the plans. A parser along with a rule description language is
also planned.

# Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Math.Symbolic.Simplify

main = do
    let x = Sym "x"
    print . simplify $ x^2 + 5 + x^2 + x*
    -- 3.0*x^2.0+5.0
```
