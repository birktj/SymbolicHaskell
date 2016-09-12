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


onlyNumeric :: (Eq a) => Math a -> Math a -> Bool
onlyNumeric sym x | sym == x = False
onlyNumeric sym (Op _ xs) = and $ onlyNumeric sym <$> xs
onlyNumeric sym _ = True

funDiff :: (Ord a, Real a, Fractional a) => Text -> Math a -> Math a
funDiff "ln" expr = 1 / expr
funDiff "sqrt"  expr = 1 / (2 * sqrt expr)
funDiff fun expr = Op "D" [Op fun [expr]]

diff' :: (Ord a, Real a, Fractional a) => Math a -> Math a -> Math a
diff' sym x | onlyNumeric sym x = 0
                     | sym == x = 1
diff' sym (Op "+" xs) = Op "+" $ diff' sym <$> xs
diff' sym (Op "*" xs) = Op "+" . fmap (Op "*") $ listMod (diff' sym) xs
diff' sym (Op "/" [x, y]) = (diff' sym x * y + diff' sym y * x) / y^2
diff' sym (Op op [x]) = funDiff op x * diff' sym x
--diff' sym (Op "^" [x, y]) | onlyNumeric sym x =
--diff' sym (Op "^" [x, y]) = y * x**(y-1) * diff' sym x
diff' sym (Op "^" [b, e]) = b**e * diff' sym (e * Op "ln" [b])
diff' sym x = Op "D" [x]



diff :: (Ord a, Real a, Fractional a) => Math a -> Math a -> Math a
diff sym = simplify
                  . diff' sym
                  . simplify
