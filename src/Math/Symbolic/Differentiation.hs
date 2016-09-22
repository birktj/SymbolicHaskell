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
funDiff "sin" expr = cos expr
funDiff "cos" expr = - sin expr
funDiff fun expr = Op "D" [Op fun [expr]]

diff' :: (Ord a, Real a, Fractional a) => Math a -> Math a -> Math a
diff' e x | onlyNumeric e x = 0
          | e == x          = 1
diff' e (Op "+" xs) = Op "+" $ diff' e <$> xs
diff' e (Op "*" xs) = Op "+" . fmap (Op "*") $ listMod (diff' e) xs
diff' e (Op "/" [x, y]) = (diff' e x * y + diff' e y * x) / y^2
diff' e (Op op [x]) = funDiff op x * diff' e x
--diff' e (Op "^" [x, y]) | onlyNumeric e x =
--diff' e (Op "^" [x, y]) = y * x**(y-1) * diff' e x
diff' e (Op "^" [b, e']) = b**e' * diff' e (e' * Op "ln" [b])
diff' e x = Op "D" [x]



diff :: (Ord a, Real a, Fractional a) => Math a -> Math a -> Math a
diff sym = simplify
                  . diff' sym
                  . simplify
