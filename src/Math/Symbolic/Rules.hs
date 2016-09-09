{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Rules where

import Math.Symbolic.Expression

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

-------------
-- Rewrite --
-------------
match :: (Eq a, Ord a) => MExpression a -> MExpression a -> Bool
match rwexpr expr | basicmatch rwexpr expr = match' (getSubExpr rwexpr expr) rwexpr expr
                  | otherwise              = False
    where
--        match' subExprs (MBiOp op x y) (MBiOp op' x' y')    = op == op' && match' subExprs x x' && match' subExprs y y'
--        match' subExprs (MUnOp op x) (MUnOp op' x')         = op == op' && match' subExprs x x'
--        match' subExprs (MMultiOp op xs) (MMultiOp op' xs') = op == op' && (all (\(x, y) -> match' subExprs x y) $ zip xs xs')
        match' subExprs (MConst c) (MConst c')              = c == c'
        match' subExprs (MNum x) (MNum y)                   = x == y
        match' subExprs (MVar i) n@(MNum _)    | T.take 2 i == "_n" = fromJust (lookup i subExprs) == n
        match' subExprs (MVar i) n@(MVar _)    | T.take 2 i == "_v" = fromJust (lookup i subExprs) == n
        match' subExprs (MVar i) n             | T.take 2 i == "_a" = fromJust (lookup i subExprs) == n
        match' subExprs (MVar x) (MVar y)                   = x == y
        match' subExprs _ _                                 = False

--        basicmatch (MBiOp op x y) (MBiOp op' x' y')    = op == op' && basicmatch x x' && basicmatch y y'
--        basicmatch (MUnOp op x) (MUnOp op' x')         = op == op' && basicmatch x x'
--        basicmatch (MMultiOp op xs) (MMultiOp op' xs') = op == op' && (all (\(x, y) -> basicmatch x y) $ zip xs xs')
        basicmatch (MOp x xs) (MOp y ys) = x == y && (and (zipWith basicmatch (sort xs) (sort ys)) || mvarall xs)
        basicmatch (MConst c) (MConst c')              = c == c'
        basicmatch (MNum x) (MNum y)                   = x == y
        basicmatch (MVar i) n@(MNum _)    | T.take 2 i == "_n" = True
        basicmatch (MVar i) n@(MVar _)    | T.take 2 i == "_v" = True
        basicmatch (MVar i) n             | T.take 2 i == "_a" = True
        basicmatch (MVar x) (MVar y)                   = x == y
        basicmatch _ _                                 = False

        mvarall = any mvara

        mvara (MVar i) | T.take 2 i == "_a" = True
        mvara _ = False


getSubExpr :: (Eq a) =>  MExpression a -> MExpression a -> [(Text, MExpression a)]
--getSubExpr (MBiOp op x y) (MBiOp op' x' y')    | op == op' = concat [getSubExpr x x', getSubExpr y y']
--getSubExpr (MUnOp op x) (MUnOp op' x')         | op == op' = getSubExpr x x'
--getSubExpr (MMultiOp op xs) (MMultiOp op' xs') | op == op' = concat $ zipWith getSubExpr xs xs'
getSubExpr (MVar i) n@(MNum _)                | T.take 2 i == "_n" = [(i, n)]
getSubExpr (MVar i) n@(MVar _)                | T.take 2 i == "_v" = [(i, n)]
getSubExpr (MVar i) n                         | T.take 2 i == "_a" = [(i, n)]
getSubExpr _ _                                             = []


rewrite :: (Eq a, Ord a) =>  MExpression a -> MExpression a -> MExpression a -> MExpression a
rewrite rwexpr substexpr expr | match rwexpr expr = substitute (getSubExpr rwexpr expr) substexpr
                              | otherwise         = expr
                              where
--                                  substitute subExprs (MBiOp op x y)   = MBiOp op (substitute subExprs x) (substitute subExprs y)
--                                  substitute subExprs (MUnOp op x)     = MUnOp op (substitute subExprs x)
--                                  substitute subExprs (MMultiOp op xs) = MMultiOp op (fmap (substitute subExprs) xs)
                                  substitute subExprs (MVar i) | T.take 1 i == "_" = fromJust (lookup i subExprs)
                                  substitute _ x                       = x

a1 = MVar "_a1"
a2 = MVar "_a2"
a3 = MVar "_a3"
a4 = MVar "_a4"
a5 = MVar "_a5"
a6 = MVar "_a6"
a7 = MVar "_a7"
a8 = MVar "_a8"
a9 = MVar "_a9"
n1 = MVar "_n1"
n2 = MVar "_n2"
n3 = MVar "_n3"
n4 = MVar "_n4"
n5 = MVar "_n5"
n6 = MVar "_n6"
n7 = MVar "_n7"
n8 = MVar "_n8"
n9 = MVar "_n9"
v4 = MVar "_v4"
v1 = MVar "_v1"
v2 = MVar "_v2"
v3 = MVar "_v3"
v5 = MVar "_v5"
v6 = MVar "_v6"
v7 = MVar "_v7"
v8 = MVar "_v8"
v9 = MVar "_v9"

x = MVar "x"
y = MVar "y"
z = MVar "z"
