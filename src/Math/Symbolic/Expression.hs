{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Expression (MExpression (..), calculate, ) where

import Data.Word
import Data.Maybe
import Data.List
import Data.Ratio
import Data.String


data MExpression a = MNum a
                   | MConst Word32
                   | MVar String
                   | MMultiOp Word32 [MExpression a]
                   | MBiOp Word32 (MExpression a) (MExpression a)
                   | MUnOp Word32 (MExpression a)
                   | MErr Word32 String
                   deriving (Eq)

-- Opperator table --
---------------------
-- +      : 0x01   --
-- -      : 0x02   --
-- *      : 0x03   --
-- /      : 0x04   --
-- abs    : 0x05   --
-- pow    : 0x06   --
-- sqrt   : 0x07   --
-- log    : 0x08   --
-- exp    : 0x09   --
-- sin    : 0x0a   --
-- cos    : 0x0b   --
-- tan    : 0x0c   --
-- asin   : 0x0d   --
-- acos   : 0x0e   --
-- atan   : 0x0f   --
-- sinh   : 0x10   --
-- cosh   : 0x11   --
-- tanh   : 0x12   --
-- asinh  : 0x13   --
-- acosh  : 0x14   --
-- atanh  : 0x15   --
-- sign   : 0x31   --
---------------------


-- Constant table --
--------------------
-- pi    : 0x21   --
-- e     : 0x22   --
--------------------

getOp :: Word32 -> String
getOp 0x01 = "+"
getOp 0x02 = "-"
getOp 0x03 = "*"
getOp 0x04 = "/"
getOp 0x05 = "abs"
getOp 0x06 = "^"
getOp 0x07 = "sqrt"
getOp 0x08 = "log"
getOp 0x09 = "exp"
getOp 0x0a = "sin"
getOp 0x0b = "cos"
getOp 0x0c = "tan"
getOp 0x0d = "asin"
getOp 0x0e = "acos"
getOp 0x0f = "atan"
getOp 0x10 = "sinh"
getOp 0x11 = "cosh"
getOp 0x12 = "tanh"
getOp 0x13 = "asinh"
getOp 0x14 = "acosh"
getOp 0x15 = "atanh"
getOp x    = "op" ++ (show x)

opCompare :: Word32 -> Word32 -> Ordering
opCompare x y | x == y = EQ
              | x < y = GT
              | otherwise = LT


parens :: MExpression a -> Bool
--parens _ = True
parens (MNum _) = False
parens (MConst _) = False
parens (MVar _) = False
parens (MBiOp 0x01 _ _) = True
parens _ = True


instance Show a => Show (MExpression a) where
    show (MNum x) = show x
    show (MConst 0x21) = "pi"
    show (MConst 0x22) = "e"
    show (MConst x) = 'C':(show x)
    show (MVar str) = str
    show (MMultiOp op xs) = (getOp op) ++ " " ++ (show $ map (show) xs)
    show (MBiOp op x y) | parens x && parens y = "(" ++ show x ++ ") " ++ getOp op ++ " (" ++ show y ++ ")"
                        | parens x = "(" ++ show x ++ ") " ++ getOp op ++ " " ++ show y
                        | parens y = show x ++ " " ++ getOp op ++ " (" ++ show y ++ ")"
                        | otherwise = show x ++ " " ++ getOp op ++ " " ++ show y
    show (MUnOp op x) | parens x  = getOp op ++ " (" ++ show x ++ ")"
                      | otherwise = getOp op ++ " " ++ show x

    show (MErr code errval) = "Error " ++ (show code) ++ ", with message: " ++ errval


instance Ord a => Ord (MExpression a) where
    compare (MBiOp op1 _ _) (MBiOp op2 _ _)   = opCompare op1 op2
    compare (MBiOp op1 _ _) (MMultiOp op2 _)  = opCompare op1 op2
    compare (MMultiOp op1 _) (MMultiOp op2 _) = opCompare op1 op2
    compare (MMultiOp op1 _) (MBiOp op2 _ _)  = opCompare op1 op2
    compare (MMultiOp _ _) _                  = GT
    compare (MBiOp _ _ _) _                   = GT
    compare (MUnOp _ x) y                     = compare x y
    compare x (MUnOp _ y)                     = compare x y
    compare _ (MMultiOp _ _)                  = LT
    compare _ (MBiOp _ _ _)                   = LT
    compare (MConst x) (MConst y) = EQ
    compare (MConst x) _ = GT
    compare _ (MConst x) = LT
    compare (MVar x) (MVar y) | x == y = EQ
                              | x < y  = GT
                              | otherwise = LT
    compare (MVar x) _ = GT
    compare _ (MVar x) = LT
    compare (MNum x) (MNum y) = compare x y
    compare _ _ = EQ

calc op x y = calculate $ MBiOp op x y

instance (Ord a, Fractional a) => Num (MExpression a) where
    x + y = calc 0x01 x y
    x - y = calc 0x02 x y
    x * y = calc 0x03 x y
    abs x = MUnOp 0x05 x
    negate x = MUnOp 0x02 x
    signum x = MUnOp 0x31 x
    fromInteger x = MNum (fromInteger x)

instance (Ord a, Fractional a) => Fractional (MExpression a) where
    x / y = calc 0x04 x y
    fromRational x = MNum (fromRational x)

instance (Ord a, Fractional a) => Floating (MExpression a) where
    pi = MConst 0x21
    exp x = MBiOp 0x06 (MConst 0x22) x
    log x = MUnOp 0x08 x
    sqrt x = MBiOp 0x07 2 x
    x ** y = MBiOp 0x06 x y
    logBase x y = MBiOp 0x08 x y
    sin x = MUnOp 0x0a x
    cos x = MUnOp 0x0b x
    tan x = MUnOp 0x0c x
    asin x = MUnOp 0x0d x
    acos x = MUnOp 0x0e x
    atan x = MUnOp 0x0f x
    sinh x = MUnOp 0x10 x
    cosh x = MUnOp 0x11 x
    tanh x = MUnOp 0x12 x
    asinh x = MUnOp 0x13 x
    acosh x = MUnOp 0x14 x
    atanh x = MUnOp 0x15 x

canCalc (MNum x) = True
canCalc _ = False

--rearrange :: MExpression a -> MExpression a
--rearrange

calculate :: (Ord a, Fractional a) => MExpression a -> MExpression a
calculate (MBiOp 0x01 (MNum x) (MNum y)) = MNum $ x + y
calculate (MBiOp 0x02 (MNum x) (MNum y)) = MNum $ x - y
calculate (MBiOp 0x03 (MNum x) (MNum y)) = MNum $ x * y
calculate (MBiOp 0x04 (MNum x) (MNum y)) = MNum $ x / y
calculate expr@(MBiOp _ (MNum _) (MNum _)) = expr

calculate (MUnOp 0x02 (MNum x)) = MNum (-x)
calculate (MBiOp op x y) | canCalc (calculate x) && canCalc (calculate y) = calculate $ MBiOp op (calculate x) (calculate y)
                         | otherwise = MBiOp op (calculate x) (calculate y)
--calculate (MUnOp op x)   | canCalc (calculate x) = calculate $ MUnOp op (calculate x)
calculate x = x
