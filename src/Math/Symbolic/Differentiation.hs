{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Differentiation where

import Math.Symbolic.Expression
import Math.Symbolic.Simplify
import Math.Symbolic.Display

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Ratio
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

listMod :: (a -> a) -> [a] -> [[a]]
listMod f xs = listMod' f xs []
    where
        listMod' f (x:xs) ys = (ys <> (f x : xs)) : listMod' f xs (ys <> [x])
        listMod' f [] ys = []


onlyNumeric :: Text -> Math a -> Bool
onlyNumeric sym (Op _ xs) = and $ onlyNumeric sym <$> xs
onlyNumeric sym (Sym x) | sym == x = False
onlyNumeric sym _ = True

funDifferentiate :: (Ord a, Real a, Fractional a) => Text -> Math a -> Math a
funDifferentiate "lg" expr = 1 / expr
funDifferentiate "sqrt"  expr = 1 / (2 * sqrt expr)
funDifferentiate fun expr = Op "D" [Op fun [expr]]

differentiate' :: (Ord a, Real a, Fractional a) => Text -> Math a -> Math a
differentiate' sym x | onlyNumeric sym x = 0
differentiate' sym (Op "+" xs) = Op "+" $ differentiate' sym <$> xs
differentiate' sym (Op "*" xs) = Op "+" . fmap (Op "*") $ listMod (differentiate' sym) xs
differentiate' sym (Op "/" [x, y]) = (differentiate' sym x * y + differentiate' sym y * x) / y^2
differentiate' sym (Op op [x]) = funDifferentiate op x * differentiate' sym x
differentiate' sym (Op "^" [x, y]) = y * x**(y-1) * differentiate' sym x
differentiate' sym (Sym x) | sym == x = 1
differentiate' sym x = Op "D" [x]



differentiate :: (Ord a, Real a, Fractional a) =>  Text -> Math a -> Math a
differentiate sym = simplify
                  . differentiate' sym
                  . simplify
