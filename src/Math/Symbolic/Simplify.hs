module Math.Symbolic.Simplify
    (
    ) where

import Math.Symbolic.Expression
import Math.Symbolic.Rules

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid


traverseMExpr f (MBiOp op x y)   = f $ MBiOp op (f $ traverseMExpr f x) (f $ traverseMExpr f y)
traverseMExpr f (MUnOp op x)     = f $ MUnOp op (f $ traverseMExpr f x)
traverseMExpr f (MMultiOp op xs) = f . MMultiOp op $ fmap (f . traverseMExpr f) xs
traverseMExpr f x = f x


simplifyRule rwmatch rwsub = traverseMExpr (rewrite (rearrange rwmatch) (rearrange rwsub))
simplifyRule' rwmatch rwsub = traverseMExpr (rewrite rwmatch rwsub)

moveSub (MNum x) | x < 0     = MUnOp 0x02 . MNum $ abs x
                 | otherwise = MNum x
moveSub x = x

simplify :: (Ord a, Fractional a) => MExpression a -> MExpression a
simplify = traverseMExpr moveSub
         . calculate
         . simplifyRule' (a1 - a1) 0
         . simplifyRule' (a1 + a1) (2 * a1)
         . simplifyRule (a1 * (a1**n1)) (a1**(n1+1))
         . simplifyRule (a1 * a1) (a1**2)
         . simplifyRule ((a1**n1) * (a1**n2)) (a1**(n1+n2))
         . simplifyRule ((n1 * a1) + (n2 * a1)) ((n1 + n2) * a1)
         . simplifyRule (a1 + (negate a2 - a3)) (a1 - a2 - a3)
         . simplifyRule (a1 + (negate a2)) (a1 - a2)
         . simplifyRule (a1 + (a1 + a2)) ((a1*2) + a2)
         . traverseMExpr moveSub
         . calculate
         . traverseMExpr rearrange


rearrange :: (Ord a, Fractional a) => MExpression a -> MExpression a
rearrange x@(MBiOp op _ _) | op == 0x01 || op == 0x02 = sumTree . sortBy (flip compare) . fmap (rearrange . calculate) $ sumList x
    where
        sumList (MBiOp 0x01 x y) = sumList x <> sumList y
        sumList (MBiOp 0x02 x y) = sumList x <> (negate <$> sumList y)
        sumList x                = [x]
        sumTree  = foldr1 (+)
    --    sumTree' = foldl1 (+)
rearrange x@(MBiOp op _ _) | op == 0x03 = multiTree . sort . fmap (rearrange . calculate) $ multiList x
    where
        multiList (MBiOp 0x03 x y) = multiList x <> multiList y
        multiList x                = [x]
        multiTree  = foldr1 (*)

rearrange x = x
