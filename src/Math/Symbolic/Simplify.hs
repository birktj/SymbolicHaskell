{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Simplify where

import Math.Symbolic.Expression
--import Math.Symbolic.Rules

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

flatten :: MExpression a -> MExpression a
flatten (MOp "+" xs) = MOp "+" . concat $ flatten' <$> xs
    where
        flatten' (MOp "+" xs) = xs
        flatten' y = [y]

flatten (MOp "*" xs) = MOp "*" . concat $ flatten' <$> xs
    where
        flatten' (MOp "*" xs) = xs
        flatten' y = [y]


{-
toAddTerm :: (Fractional a, Ord a) => MExpression a -> MExpression a
toAddTerm (MOp "*" [n@(MNum _), op@(MOp _ _)]) = n * toAddTerm op
toAddTerm t@(MOp "*" [MNum n, x]) = t
toAddTerm x@(MVar _) = 1 * x
toAddTerm (MOp op xs) = MOp op $ fmap toAddTerm xs
toAddTerm x = x
-}

toMTerm :: (Fractional a, Ord a) => MExpression a -> MExpression a
toMTerm t@(MOp "^" _) = t
toMTerm x@(MVar _)    = x**1
toMTerm (MOp op xs)   = MOp op $ fmap toMTerm xs
toMTerm x = x

likeMTerm :: (Fractional a, Ord a) => MExpression a -> MExpression a -> Bool
likeMTerm x y | x == y = True
likeMTerm (MOp "^" [x1, y1]) (MOp "^" [x2, y2]) = x1 == x2
likeMTerm _ _ = False

getExponent :: MExpression a -> MExpression a
getExponent (MOp "^" [x, y]) = y
getExponent _ = error "Not a power"

getBase :: MExpression a -> MExpression a
getBase (MOp "^" [x, y]) = x
getBase _ = error "Not a power"

getLikeM :: (Fractional a, Ord a) => MExpression a -> MExpression a
getLikeM (MOp "*" xs) = MOp "+" . fmap (\x -> getBase (head x) ** sum (fmap getExponent x)) . groupBy likeMTerm . sort $ fmap getLikeM xs
getLikeM (MOp op xs) = MOp op $ fmap getLikeM xs
getLikeM x = x

getLike :: (Fractional a, Ord a) => MExpression a -> MExpression a
getLike (MOp "+" xs) = MOp "+" . fmap (\x -> head x * fromIntegral (length x)) . group . sort $ fmap getLike xs
getLike (MOp op xs) = MOp op $ fmap getLike xs
getLike x = x
