{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Simplify where

import Math.Symbolic.Expression

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Ratio
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)


isOp :: Text -> Math a -> Bool
isOp x (Op y _) = x == y
isOp _ _ = False

takeOp :: Text -> [Math a] -> ([Math a], Math a)
takeOp op ms = takeOp' ms []
    where
        takeOp' (x:xs) ys | isOp op x = (xs <> ys, x)
                          | otherwise = takeOp' xs $ x:ys



converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys


runFun :: (Eq a) => (a -> a) -> a -> a
runFun f a = converge (==) $ iterate f a


opCompare :: Text -> Text -> Ordering
opCompare x y | percedende x == percedende y = EQ
              | percedende x > percedende y  = GT
              | otherwise                    = LT


mathCompare :: (Ord a) => Math a -> Math a -> Ordering
mathCompare (Op "^" [x, xs]) (Op "^" [y, ys]) = mathCompare (mathSort xs) (mathSort ys) `ifEq` mathCompare x y
mathCompare (Op "*" [Numeric x, xs])  (Op "*" [Numeric y, ys]) = mathCompare (mathSort xs) (mathSort ys) `ifEq` compare y x
mathCompare (Op "*" [Numeric _, x]) y = mathCompare x y
mathCompare x (Op "*" [Numeric _, y]) = mathCompare x y
mathCompare (Op x xs) (Op y ys) = opCompare y x `ifEq` foldr ifEq EQ (zipWith mathCompare xs ys)
mathCompare Op{} _ = LT
mathCompare _ Op{} = GT
mathCompare (Sym x) (Sym y) = compare x y
mathCompare Sym{} _ = LT
mathCompare _ Sym{} = GT
mathCompare (Numeric x) (Numeric y) = compare y x


multiCompare :: (Ord a) => Math a -> Math a -> Ordering
multiCompare (Numeric x) (Numeric y) = compare y x
multiCompare (Numeric _) _ = LT
multiCompare _ (Numeric _) = GT
multiCompare x y = mathCompare x y

mathSort :: (Ord a) => Math a -> Math a
mathSort (Op "+" xs) = Op "+" . sortBy mathCompare $ mathSort <$> xs
mathSort (Op "*" xs) = Op "*" . sortBy multiCompare $ mathSort <$> xs
mathSort (Op op xs)  = Op op $ mathSort <$> xs
mathSort x = x


level :: (Ord a) => Math a -> Math a
level (Op "+" xs) = Op "+" . fmap level . concat $ level' <$> xs
    where
        level' (Op "+" xs) = xs
        level' y = [y]

level (Op "*" xs) = Op "*" . fmap level . concat $ level' <$> xs
    where
        level' (Op "*" xs) = xs
        level' y = [y]


level (Op op xs) = Op op $ level <$> xs

level x = x

simplifyRational :: (Ord a, Fractional a, Real a) => Math a -> Math a
simplifyRational (Op "/" [Op "/" [a, b], c]) = a / (b*c)
simplifyRational (Op "/" [a, Op "/" [b, c]]) = (a*b) / c
simplifyRational (Op "*" xs) | any (isOp "/") xs = let (as, Op "/" [b, c]) = takeOp "/" xs
                                                   in Op "*" (b:as) / c
simplifyRational (Op op xs) = Op op $ simplifyRational <$> xs
simplifyRational x = x



toMTerm :: (Ord a, Fractional a, Real a) => Math a -> Math a
toMTerm (Op "*" xs) = Op "*" $ toMTerm' . toMTerm <$> xs
    where
        toMTerm' x@(Op "^" _)  = x
    --    toMTerm' x@(Numeric _) = x
        toMTerm' x = x**1
toMTerm (Op "/" [x, y]) = (toMTerm . level $ Op "*" [x]) / (toMTerm . level $ Op "*" [y])
toMTerm (Op op xs)  = Op op $ toMTerm <$> xs
toMTerm x = x


toSTerm :: (Ord a, Fractional a, Real a) => Math a -> Math a
toSTerm (Op "+" xs) = Op "+" $ toSTerm' . toSTerm <$> xs
    where

        toSTerm' x@(Op "*" (Numeric _:_))  = x
        toSTerm' (Op "*" xs)  = Op "*" $ 1:xs
    --    toSTerm' x@(Numeric _) = x
        toSTerm' x = 1*x
toSTerm (Op op xs)  = Op op $ toSTerm <$> xs
toSTerm x = x


likeMTerm :: (Ord a, Fractional a, Real a) => Math a -> Math a -> Bool
likeMTerm (Op "^" [x, xs]) (Op "^" [y, ys]) = x == y
likeMTerm _ _ = False


likeSTerm :: (Ord a, Fractional a, Real a) => Math a -> Math a -> Bool
likeSTerm (Op "*" (Numeric _ :xs)) (Op "*" (Numeric _:ys)) = xs == ys
likeSTerm _ _ = False


collectMLike :: (Ord a, Fractional a, Real a) => Math a -> Math a
collectMLike (Op "*" xs) = Op "*" . concatMap addlike . groupBy likeMTerm $ collectMLike <$> xs
    where
        addlike (Op "^" [x, y] : xs) = [x ** foldr1 (+) (y:fmap getExponent xs)]
        addlike x = x
        getExponent (Op "^" [_, y]) = y

collectMLike (Op "/" [Op "*" x, Op "*" y]) = Op "*" x' / Op "*" y'
    where
        (x', y') = reduceFraction (collectMLike <$> x) (collectMLike <$> y)
        reduceFraction (x:xs) ys = let y' = find (likeMTerm x) ys
                                   in case y' of
                                       Just y -> let (x'', y'') = diffMterm x y
                                                     (xs', ys') = reduceFraction xs ys
                                                 in (x'':xs', y'': delete y ys')
                                       Nothing -> let (xs', ys') = reduceFraction xs ys
                                                  in (x:xs', ys')
        reduceFraction [] ys = ([], ys)
        diffMterm (Op "^" [x, xs]) (Op "^" [y, ys]) = (x**(xs-ys), 1 {-y**(ys-(xs-ys))-})

collectMLike (Op op xs) = Op op $ collectMLike <$> xs
collectMLike x = x


collectSLike :: (Ord a, Fractional a, Real a) => Math a -> Math a
collectSLike (Op "+" xs) = Op "+" . concatMap addlike . groupBy likeSTerm $ collectSLike <$> xs
    where
        addlike (Op "*" (x@(Numeric _):xs) : ys) = [foldr1 (+) (x:fmap getNumeric ys) * (foldr1 (*) xs)]
        addlike x = x
        getNumeric (Op "*" (x@(Numeric _):_)) = x

collectSLike (Op op xs) = Op op $ collectSLike <$> xs
collectSLike x = x


reduce :: (Ord a, Fractional a, Real a) => Math a -> Math a
reduce (Op "+" [x]) = reduce x
reduce (Op "*" [x]) = reduce x
reduce (Op "^" [x, 1]) = reduce x
reduce (Op "^" [x, 0]) = Numeric 1
reduce (Op "*" xs) | 0 `elem` xs = Numeric 0
                   | otherwise = case filter (/=1) xs of
                       [] -> Numeric 1
                       [x] -> x
                       xs -> Op "*" $ reduce <$> xs
reduce (Op "/" [0, _]) = Numeric 0
reduce (Op "/" [x, 1]) = x
reduce (Op "+" xs) = case xs' of
    [] -> Numeric 0
    [a] -> a
    a -> Op "+" a
    where
        xs' = concatMap removeNull $ reduce <$> xs
        removeNull (Numeric 0) = []
        removeNull x = [x]

reduce (Op op xs) = foldConst . Op op $ reduce <$> xs
reduce x = x


foldConst :: (Ord a, Real a, Fractional a) => Math a -> Math a
foldConst (Op "+" xs) = Op "+" . concatMap (\x -> if all isNumeric x then [Numeric . sum $ getNumeric <$> x] else x) . groupBy sameType $ foldConst <$> xs
foldConst (Op "*" xs) = Op "*" . concatMap (\x -> if all isNumeric x then [Numeric . product $ getNumeric <$> x] else x) . groupBy sameType $ foldConst <$> xs
foldConst (Op "/" [x, y]) = foldConst' (foldConst x) (foldConst y)
    where
        foldConst' (Numeric x) (Numeric y) = Numeric $ x / y
        foldConst' (Op "*" (Numeric x : xs)) (Op "*" (Numeric y : ys)) = Op "*" (Numeric (x / y) : xs) / (Op "*" ys) 
foldConst (Op "^" [Numeric x, Numeric y]) | denominator (toRational y) == 1 = Numeric $ x ^ numerator (toRational y)
foldConst (Op op xs) = Op op $ foldConst <$> xs
foldConst x = x


isNumeric Numeric{} = True
isNumeric (Op "-" [Numeric{}]) = True
isNumeric _ = False
getNumeric (Numeric x) = x
--getNumeric (Op "-" [Numeric x]) = -x
getNumeric _ = error "not a numeric"

sameType :: Math a -> Math a -> Bool
sameType Numeric{} Numeric{} = True
--sameType (Op "-" [x]) y = sameType x y
--sameType ()
sameType Sym{} Sym{}         = True
sameType (Op x _) (Op y _)   = x == y
sameType _ _ = False


expand :: (Ord a, Fractional a, Real a) => Math a -> Math a
expand (Op "*" xs) = reduce . Op "+" . fmap (Op "*") . mapM fromSum $ expand <$> xs
    where
        fromSum (Op "+" xs) = xs
        fromSum x = [x]
expand (Op "^" [Op "*" xs, y]) = Op "*" $ (\x -> Op "^" [x, y]) . expand <$> xs
expand (Op op xs) = Op op $ expand <$> xs
expand a = a


likeTerms :: (Ord a, Fractional a, Real a) => Math a -> Math a
likeTerms = runFun reduce
          . collectSLike
          . toSTerm
          . runFun reduce
          . collectMLike
          . toMTerm


simplify :: (Ord a, Real a, Fractional a) => Math a -> Math a
simplify = runFun simplify'
    where
        simplify' = mathSort
                  . expand
                  . foldConst
                  . runFun simplifyRational
                  . runFun level
                  . likeTerms
                  . runFun reduce
                  . mathSort
                  . runFun simplifyRational
                  . mathSort
                  . runFun level
                 -- . toSTerm

x = Sym "x"
