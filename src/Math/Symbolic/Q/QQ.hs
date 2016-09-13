{-# LANGUAGE OverloadedStrings, TemplateHaskell, ViewPatterns, QuasiQuotes #-}
module Math.Symbolic.Q.QQ where

import Math.Symbolic.Expression
import Math.Symbolic.Q.Parser

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
        expr <- parseExpr pos s :: Monad m => m (Math Rational)
        dataToExpQ (const Nothing `extQ` antiExprExp `extQ` handleTextE) expr


handleTextE :: T.Text -> Maybe TH.ExpQ
handleTextE x = Just . TH.appE (TH.varE 'T.pack) . TH.litE . TH.StringL $ T.unpack x


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
        expr <- parseExpr pos s :: Monad m => m (Math Rational)
        dataToPatQ (const Nothing `extQ` antiExprPat `extQ` handleTextP) expr


handleTextP :: T.Text -> Maybe (TH.Q TH.Pat)
handleTextP x = Just . TH.viewP (TH.varE 'T.unpack) . TH.litP . TH.StringL $ T.unpack x


antiExprPat ::  Math Rational -> Maybe (TH.Q TH.Pat)
antiExprPat  (Op "numM" [Sym v])  = Just $ TH.conP (TH.mkName "Numeric")
                                                 [TH.varP (TH.mkName $ T.unpack v)]
antiExprPat  (Op "symM" [Sym v])  = Just $ TH.conP (TH.mkName "Sym")
                                                 [TH.varP (TH.mkName $ T.unpack v)]
antiExprPat  (Op "exprM" [Sym v]) = Just $ TH.varP (TH.mkName $ T.unpack v)
antiExprPat  _                = Nothing
