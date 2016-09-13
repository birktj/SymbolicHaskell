{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Math.Symbolic.Expression (Math(..), toTree) where

import Data.Word
import Data.Maybe
import Data.List
import Data.Ratio
import Data.String
import Data.Monoid
import Data.Generics (Typeable, Data)
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)


data Math a = Numeric a
            | Sym Text
            | Op Text [Math a]
            deriving (Eq, Typeable, Data)



percedende :: Text -> Int
percedende "+" = 1
percedende "-" = 1
percedende "*" = 2
percedende "/" = 2
percedende "^" = 3
percedende _ = 3

instance (Show a, Fractional a, Real a, Ord a) => Show (Math a) where
    show = showMath . toTree


toTree :: (Show a, Real a, Fractional a, Ord a) => Math a -> Math a
toTree (Op "*" xs) = foldr1 (*) $ toTree <$> xs
toTree (Op "+" xs) = foldl1 sumTree $ toTree <$> xs
    where
        sumTree y (Op "*" [Numeric (-1), x]) = Op "-" [y, x]
        sumTree y (Op "*" (Numeric x:xs)) | x < 0 = Op "-" [y, Op "*" $ (Numeric $ abs x):xs]
        sumTree y (Numeric x) | x < 0 = Op "-" [y, Numeric $ abs x]
        sumTree x y = x + y
toTree (Op op x) = Op op $ toTree <$> x
toTree x = x

parens :: (Show a, Fractional a, Ord a) => Int -> Math a -> String
parens p (Op "*" [Numeric (-1), x]) | 1 > p = "-(" <> showMath x <> ")"
                         -- | otherwise = "(" <> showMath x <> ")"
parens p x@(Op op _) | percedende op < p = "(" <> showMath x <> ")"
parens _ (Numeric x) | x < 0 = "-" <> show (abs x)
parens _ x = showMath x

isNeg :: (Show a, Fractional a, Ord a) => Math a -> Bool
isNeg (Op "*" (Numeric (-1):_)) = True
isNeg (Numeric x) = x < 0
isNeg _ = False

getNeg :: (Show a, Fractional a, Ord a) => Math a -> Math a
getNeg (Op "*" [_, x]) = x
getNeg x = x

sig :: (Show a, Fractional a, Ord a) => Math a -> String
sig (Op "*" (Numeric (-1):_)) = "-"
sig (Numeric x) | x < 0 = "-"
sig _ = ""

showMath :: (Show a, Fractional a, Ord a) => Math a -> String
showMath (Op op [x]) = T.unpack  op <> "(" <> showMath x <> ")"
showMath (Op op xs) = intercalate (T.unpack op) $ parens (percedende op) <$> xs
showMath (Sym x) = T.unpack x
showMath (Numeric x) = show $ abs x


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

instance (Ord a, Real a, Fractional a) => Num (Math a) where
    x + y = Op "+" [x, y]
    x - y = Op "+" [x, Numeric (-1) * y]
    x * y = Op "*" [x, y]
    abs x    = Op "abs" [x]
    negate x = Numeric (-1) * x
    signum x = Op "signum" [x]
    fromInteger x = Numeric (fromInteger x)

instance (Ord a, Real a, Fractional a) => Fractional (Math a) where
    x / y = Op "/" [x, y]
    fromRational x = Numeric (fromRational x)

instance (Ord a, Real a, Fractional a) => Floating (Math a) where
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
