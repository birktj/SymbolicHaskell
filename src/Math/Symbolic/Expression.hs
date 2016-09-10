{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Expression where

import Data.Word
import Data.Maybe
import Data.List
import Data.Ratio
import Data.String
import Data.Monoid
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)


data Math a = Numeric a
            | Op Text [Math a]
            | Sym Text
            deriving (Eq)


instance Show a => Show (Math a) where
    show (Numeric x) = show x
    show (Sym str) = T.unpack str
    show (Op op xs) = T.unpack op <> show xs


ifEq :: Ordering -> Ordering -> Ordering
ifEq EQ a = a
ifEq a  _ = a

instance (Ord a) => Ord (Math a) where
    compare (Op x xs) (Op y ys) = compare x y `ifEq` compare xs ys
    compare Op{} _ = GT
    compare _ Op{} = LT
    compare (Sym x) (Sym y) = compare x y
    compare Sym{} _ = GT
    compare _ Sym{} = LT
    compare (Numeric x) (Numeric y) = compare x y

instance (Ord a, Fractional a) => Num (Math a) where
    x + y = Op "+" [x, y]
    x - y = Op "+" [x, Numeric (-1) * y]
    x * y = Op "*" [x, y]
    abs x    = Op "abs" [x]
    negate x = Op "-" [x]
    signum x = Op "signum" [x]
    fromInteger x = Numeric (fromInteger x)

instance (Ord a, Fractional a) => Fractional (Math a) where
    x / y = Op "/" [x, y]
    fromRational x = Numeric (fromRational x)

instance (Ord a, Fractional a) => Floating (Math a) where
    x ** y = Op "^" [x, y]

    pi = Sym "pi"

    exp x = Sym "e" ** x
    sqrt x = Op "sqrt" [x]
    log x = Op "log" [x]
    logBase x y = Op "logBase" [x, y]

    sin x = Op "sin" [x]
    cos x = Op "cos" [x]
    tan x = Op "tan" [x]

    asin x = Op "asin" [x]
    acos x = Op "acos" [x]
    atan x = Op "atan" [x]

    sinh x = Op "sinh" [x]
    cosh x = Op "cosh" [x]
    tanh x = Op "tanh" [x]

    asinh x = Op "asinh" [x]
    acosh x = Op "acosh" [x]
    atanh x = Op "atanh" [x]



a = Sym "a"
b = Sym "b"
c = Sym "c"
d = Sym "d"
e = Sym "e"
f = Sym "f"
