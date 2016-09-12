# SymbolicHaskell

A simple Haskell CAS DSL.

Symbolic Haskell aims to provide a DSL for symbolic calculations in Haskell.

# Features

Currently the system supports basic simplification and differentiation.
SymbolicHaskell simplifies in a way resembling Yacas in that expressions are
expanded before simplification and the result is not always simpler.

# Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Math.Symbolic.Expression
import Math.Symbolic.Display
import Math.Symbolic.Simplify
import Math.Symbolic.Differentiation


main = do
    let x    = Sym "x"
        expr = x^2 + 5 + x^2 + x*x

    print expr
    -- x*x+5.0+x*x+x*x

    print $ simplify expr
    -- 3.0*x^2.0+5.0

    print $ diff x expr
    -- 6.0*x

    putStrLn . showMathAST $ simplify expr
    -- [+ [* 3.0, [^ x, 2.0]], 5.0]
```

# Internals

Much of SymbolicHaskell is based on the techniques described
[here](http://www.math.wpi.edu/IQP/BVCalcHist/calc5.html). Internally
SymbolicHaskell only uses 3 types, operators (mathematical functions are also
seen as operators), numbers and symbols. All operators store their operands in
lists. Operators like addition and multiplication are leveled out making working
on the syntax tree simpler, however this also leads to some problems, making
rewrite rules for example becomes much harder. Displaying negatives properly can
also be a challenge.

# TODO
- Improve simplification rules
- Improve differentiation
- Improve Show instance
- Create a parser
- Create a simpler way of defining rewrite rules
- Create an evaluation monad
- Create a latex renderer
- Add simple integrals
