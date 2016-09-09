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


data MExpression a = MNum a
                   | MOp Text [MExpression a]
                   | MTerm a (MExpression a) a
                   | MConst Text
                   | MVar Text
                --  | MErr Word32 Text
                   deriving (Eq)



parens :: MExpression a -> Bool
--parens _ = True
parens (MNum _) = False
parens (MConst _) = False
parens (MVar _) = False
parens _ = True

instance Show a => Show (MExpression a) where
    show (MNum x) = show x
    show (MConst "pi") = "pi"
    show (MConst "e") = "e"
    show (MConst x) = 'C' : T.unpack x
    show (MVar str) = T.unpack str
    show (MTerm n x p) = show n <> "*" <> show x <> "^" <> show p
    show (MOp op xs) = T.unpack op <> show xs
    --show (MMultiOp op xs) = (getOp op) ++ " " ++ (show $ map (show) xs)
    --show (MBiOp op x y) | parens x && parens y = "(" ++ show x ++ ") " ++ getOp op ++ " (" ++ show y ++ ")"
    --                    | parens x = "(" ++ show x ++ ") " ++ getOp op ++ " " ++ show y
    --                    | parens y = show x ++ " " ++ getOp op ++ " (" ++ show y ++ ")"
    --                    | otherwise = show x ++ " " ++ getOp op ++ " " ++ show y
    --show (MUnOp op x) | parens x  = getOp op ++ " (" ++ show x ++ ")"
    --                  | otherwise = getOp op ++ " " ++ show x

--    show (MErr code errval) = "Error " <> show code <> ", with message: " <> T.unpack errval


isInfix :: Text -> Bool
isInfix x = x `elem` ["+", "*", "/", "^"]

percedence :: Text -> Int
percedence "+" = 1
percedence "*" = 2
percedence "/" = 2
percedence "^" = 3
percedence _   = -1

ifEq :: Ordering -> Ordering -> Ordering
ifEq EQ a = a
ifEq a _  = a

getOrd :: [Ordering] -> Ordering
getOrd (EQ:xs) = getOrd xs
getOrd [] = EQ
getOrd (a:_) = a

instance Ord a => Ord (MExpression a) where
    compare (MOp "-" [x]) y = compare x y
    compare x (MOp "-" [y]) = compare x y

    compare (MOp "^" [x1, x2]) (MOp "^" [y1, y2]) = compare x2 y2 `ifEq` compare x1 y1

    compare (MOp x xs) (MOp y ys) | isInfix x && isInfix y = compare (percedence y) (percedence x)
                                                             `ifEq` getOrd (zipWith compare xs ys)
                                  | otherwise = compare x y `ifEq` compare xs ys

    compare MOp{} _ = GT
    compare _ MOp{} = LT

    compare (MTerm n1 x1 p1) (MTerm n2 x2 p2) = compare p1 p2 `ifEq` (compare x1 x2 `ifEq` compare n1 n2)
    compare MTerm{} _ = GT
    compare _ MTerm{} = LT

    compare (MConst x) (MConst y) = EQ
    compare (MConst x) _ = GT
    compare _ (MConst x) = LT
    compare (MVar x) (MVar y) = compare y x
    compare (MVar x) _ = GT
    compare _ (MVar x) = LT
    compare (MNum x) (MNum y) = compare x y


flatten :: (Ord a) => MExpression a -> MExpression a
flatten (MOp "+" xs) = MOp "+" . (sortBy $ flip compare) . concat $ flatten' <$> xs
    where
        flatten' (MOp "+" xs) = xs
        flatten' y = [y]

flatten (MOp "*" xs) = MOp "*" . sort . concat $ flatten' <$> xs
    where
        flatten' (MOp "*" xs) = xs
        flatten' y = [y]


getTerm :: (Num a) => MExpression a -> MExpression a
getTerm (MOp "*" [MNum n, MTerm n' x p]) = MTerm (n * n') x p
getTerm (MOp "*" [MNum n, MOp "^" [x, MNum p]]) = MTerm n x p
getTerm (MOp "*" [MNum n, x]) = MTerm n x 1
getTerm (MOp "^" [x, MNum p]) = MTerm 1 x p
getTerm x = x

traverseM :: (MExpression a -> MExpression a) -> MExpression a -> MExpression a
traverseM f (MOp x xs) = f . MOp x $ traverseM f <$> xs
traverseM f e = f e

eqTerm :: (Eq a) => MExpression a -> MExpression a -> Bool
eqTerm (MTerm _ x p1) (MTerm _ y p2) = x == y && p1 == p2
eqTerm MNum{} MNum{} = True
eqTerm (MConst x) (MConst y) = x == y
eqTerm (MVar x) (MVar y) = x == y
eqTerm (MOp x xs) (MOp y ys) = x == y && and (zipWith eqTerm xs ys)
eqTerm _ _ = False

instance (Ord a, Fractional a) => Num (MExpression a) where
    x + y = calculate . flatten $ MOp "+" [x, y]
    x - y = calculate . flatten $ MOp "+" [x, negate y]
    (MOp "/" [a, b]) * c = calculate $ (a*c) / b
    a * (MOp "/" [b, c]) = calculate $ (a*b) / c
    x * y = calculate . flatten $ MOp "*" [x, y]
    abs x    = MOp "abs" [x]
    negate x = MOp "-" [x]
    signum x = MOp "signum" [x]
    fromInteger x = MNum (fromInteger x)

instance (Ord a, Fractional a) => Fractional (MExpression a) where
    (MOp "/" [x1, y1]) / (MOp "/" [x2, y2]) = calculate $ MOp "/" [x1*x2, y1*y2]
    (MOp "/" [a, b]) / c = calculate $ MOp "/" [a, b*c]
    a / (MOp "/" [b, c]) = calculate $ MOp "/" [a*b, c]
    x / y = MOp "/" [x, y]
    fromRational x = MNum (fromRational x)

instance (Ord a, Fractional a) => Floating (MExpression a) where
    pi = MConst "pi"
    exp x = MOp "^" [MConst "e", x]
    log x = MOp "log" [x]
    sqrt x = MOp "sqrt" [x]
    x ** y = MOp "^" [x, y]
    logBase x y = MOp "logBase" [x, y]
    sin x = MOp "sin" [x]
    cos x = MOp "cos" [x]
    tan x = MOp "tan" [x]
    asin x = MOp "asin" [x]
    acos x = MOp "acos" [x]
    atan x = MOp "atan" [x]
    sinh x = MOp "sinh" [x]
    cosh x = MOp "cosh" [x]
    tanh x = MOp "tanh" [x]
    asinh x = MOp "asinh" [x]
    acosh x = MOp "acosh" [x]
    atanh x = MOp "atanh" [x]

isNumeric :: (MExpression a) -> Bool
isNumeric (MNum _) = True
isNumeric (MOp "-" [MNum _]) = True
isNumeric _ = False


sameType :: MExpression a -> MExpression a -> Bool
sameType x y = isNumeric x && isNumeric y


extractNumeric :: (Num a) => MExpression a -> a
extractNumeric (MNum a) = a
extractNumeric (MOp "-" [MNum x]) = -x
extractNumeric _ = error "Not a numeric"

calculate :: (Fractional a, Ord a) => MExpression a -> MExpression a
calculate (MOp "+" xs) | all isNumeric $ fmap calculate xs = MNum . sum $ fmap (extractNumeric . calculate) xs
calculate (MOp "+" xs)  = MOp "+" . concatMap (\x -> if all isNumeric x then [MNum . sum $ fmap extractNumeric x] else x) . groupBy sameType . sort $ fmap calculate xs

calculate (MOp "*" xs) | all isNumeric $ fmap calculate xs = MNum . product $ fmap (extractNumeric . calculate) xs
calculate (MOp "*" xs)  = MOp "*" . concatMap (\x -> if all isNumeric x then [MNum . product $ fmap extractNumeric x] else x) . groupBy sameType . sort $ fmap calculate xs

calculate (MOp "/" xs) | all isNumeric $ fmap calculate xs = MNum . foldr (/) 1 $ fmap (extractNumeric . calculate) xs
calculate (MOp "/" xs)  = MOp "/" . concatMap (\x -> if all isNumeric x then [MNum . foldr (/) 1 $ fmap extractNumeric x] else x) . groupBy sameType . sort $ fmap calculate xs

calculate (MOp op xs) = MOp op $ fmap calculate xs
calculate x = x



a = MVar "a"
b = MVar "b"
c = MVar "c"
d = MVar "d"
e = MVar "e"
f = MVar "f"
