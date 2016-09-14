{-# LANGUAGE OverloadedStrings, TemplateHaskell, ViewPatterns, QuasiQuotes #-}
module Math.Symbolic.M.QQ (sym) where

import Math.Symbolic.Expression
import Math.Symbolic.Simplify (level, runFun)
import Math.Symbolic.M.Parser
import Math.Symbolic.Display

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Ratio
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

import Data.Generics
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote




sym  :: QuasiQuoter
sym  =  QuasiQuoter { quoteExp = quoteExprExp,
                       quotePat = quoteExprPat,
                       quoteType = undefined,
                       quoteDec  = undefined
                   }


quoteExprExp :: String -> TH.ExpQ
quoteExprExp s = do
        loc <- TH.location
        let pos = (TH.loc_filename loc,
                   fst (TH.loc_start loc),
                   snd (TH.loc_start loc))
        expr <- parseExpr pos s :: TH.Q (Math Rational) -- :: (Fractional a, Real a, Data a, Typeable a) => TH.Q (Math a)
        dataToExpQ (const Nothing `extQ` antiExprExp `extQ` handleTextE `extQ` handleTypeE) expr


handleTextE :: T.Text -> Maybe TH.ExpQ
handleTextE x = Just . TH.appE (TH.varE 'T.pack) . TH.litE . TH.StringL $ T.unpack x

handleTypeE :: Rational -> Maybe TH.ExpQ
handleTypeE x = Just . TH.appE (TH.varE 'fromRational) . TH.litE $ TH.RationalL x

antiExprExp :: Math Rational -> Maybe (TH.Q TH.Exp)
antiExprExp  (Op "numM" [Sym v])  = Just $ TH.appE  (TH.conE (TH.mkName "Numeric"))
                                                (TH.varE (TH.mkName $ T.unpack v))
antiExprExp  (Op "symM" [Sym v])  = Just $ TH.appE  (TH.conE (TH.mkName "Symbolic"))
                                                (TH.varE (TH.mkName $ T.unpack v))
antiExprExp  (Op "exprM" [Sym v]) = Just $ TH.varE  (TH.mkName $ T.unpack v)
antiExprExp  _                = Nothing


quoteExprPat :: String -> TH.PatQ
quoteExprPat s =  do
        loc <- TH.location
        let pos =  (TH.loc_filename loc,
                   fst (TH.loc_start loc),
                   snd (TH.loc_start loc))
        expr <- parseExpr pos s :: TH.Q (Math Rational) --  :: (Fractional a, Real a, Data a, Typeable a) => TH.Q (Math a)
    --    expr' <- [|expr|]
        dataToPatQ (const Nothing `extQ` antiExprPat `extQ` handleTextP `extQ` handleTypeP) expr -- (expr :: (Fractional a, Real a, Data a, Typeable a) => Math a)
        --TH.sigP (dataToPatQ (const Nothing `extQ` antiExprPat `extQ` handleTextP) expr) (TH.appT (TH.conT $ TH.mkName "Math" ) . TH.varT $ TH.mkName "a")


handleTextP :: T.Text -> Maybe (TH.Q TH.Pat)
handleTextP x = Just . TH.viewP (TH.varE 'T.unpack) . TH.litP . TH.StringL $ T.unpack x

handleTypeP :: Rational -> Maybe TH.PatQ
handleTypeP x = Just . TH.viewP (TH.varE 'toRational) . TH.litP $ TH.RationalL x

antiExprPat ::  Math Rational -> Maybe (TH.Q TH.Pat)
antiExprPat (Op "numM" [Sym v])  = Just $ TH.conP (TH.mkName "Numeric")
                                                 [TH.varP (TH.mkName $ T.unpack v)]
antiExprPat (Op "symM" [Sym v])  = Just $ TH.conP (TH.mkName "Sym")
                                                 [TH.varP (TH.mkName $ T.unpack v)]
antiExprPat (Op "exprM" [Sym v]) = Just $ TH.varP (TH.mkName $ T.unpack v)
antiExprPat (Op "listM" [Sym v]) = Just $ TH.varP (TH.mkName $ T.unpack v)
antiExprPat (Op "+:" xs) = Just $ do
    let (Op "+" xs'') = level' (Op "+" xs)
    xs' <- mapM (dataToPatQ (const Nothing `extQ` antiExprPat `extQ` handleTextP `extQ` handleTypeP)) xs''
    TH.viewP (TH.varE 'level') $ TH.conP (TH.mkName "Op") [TH.viewP (TH.varE 'T.unpack) . TH.litP $ TH.StringL "+",
                                        return $ foldr1 (\x y -> TH.UInfixP x (TH.mkName ":") y) xs']
antiExprPat (Op "*:" xs) = Just $ do
    let (Op "*" xs'') = level' (Op "*" xs)
    xs' <- mapM (dataToPatQ (const Nothing `extQ` antiExprPat `extQ` handleTextP `extQ` handleTypeP)) xs''
    TH.viewP (TH.varE 'level') $ TH.conP (TH.mkName "Op") [TH.viewP (TH.varE 'T.unpack) . TH.litP $ TH.StringL "*",
                                        return $ foldr1 (\x y -> TH.UInfixP x (TH.mkName ":") y) xs']


antiExprPat  _                = Nothing


level' :: (Ord a) => Math a -> Math a
level' = runFun level
