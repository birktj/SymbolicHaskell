{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Display where


import Math.Symbolic.Expression

import Data.Word
import Data.Maybe
import Data.List
import Data.Ratio
import Data.String
import Data.Monoid
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

parens :: (Show a, Fractional a, Ord a) => Int -> Math a -> String
parens p (Op "*" [-1, x]) | 1 < p = "(" <> showMath x <> ")"
parens p x@(Op op _) | percedende op < p = "(" <> showMath x <> ")"
parens _ x = showMath x

isNeg :: (Show a, Fractional a, Ord a) => Math a -> Bool
isNeg (Op "*" (-1:_)) = True
isNeg _ = False

getNeg :: (Show a, Fractional a, Ord a) => Math a -> Math a
getNeg (Op "*" [_, x]) = x
getNeg x = x

showMath :: (Show a, Fractional a, Ord a) => Math a -> String
showMath (Op "*" [-1, x]) = "-" <> parens 1 x
showMath (Op "+" xs) = showSum xs
    where
        showSum (x:y:xs) | isNeg y = showMath x <> "-" <> showSum ( y : xs)
                         | otherwise = showMath x <> "+" <> showSum ( y : xs)
        showSum [x] = showMath x
        showSum [] = ""
showMath (Op op [x]) = T.unpack  op <> "(" <> showMath x <> ")"
showMath (Op op xs) = intercalate (T.unpack op) $ parens (percedende op) <$> xs
showMath (Sym x) = T.unpack x
showMath (Numeric x) = show x
