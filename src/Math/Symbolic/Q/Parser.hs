{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Q.Parser where

import Math.Symbolic.Expression

import Data.Word
import Data.Maybe
import Data.List
import Data.Monoid
import Data.Ratio
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)

import Numeric
import Data.Functor.Identity

import Text.ParserCombinators.Parsec hiding (spaces)

import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)


lexer = makeTokenParser javaStyle

binary  name fun       = Infix (do{ reservedOp lexer name; return fun })
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })


msymbol :: Parser Char
msymbol = oneOf "'"

spaces :: Parser ()
spaces = skipMany space

parseIntNum :: (Fractional a) => Parser (Math a)
parseIntNum = (Numeric . fromIntegral . read)
          <$> many1 digit

parseFloatNum :: (Fractional a) => Parser (Math a)
parseFloatNum = (\f r -> Numeric . fromRational . fst . head . readDec $ f ++ '.':r)
            <$> (many1 digit <* char '.')
            <*> many1 digit

parseConst :: (Fractional a) => Parser (Math a)
parseConst = (\c n  -> Sym . T.pack $ c:n)
          <$> letter
          <*> many (letter <|> digit <|> msymbol)


parseFunc :: (Fractional a, Real a) => Parser (Math a)
parseFunc = (\c n vs -> Op (T.pack $ c:n) vs)
          <$> letter
          <*> many (letter <|> digit <|> msymbol)
          <*> parens lexer (commaSep lexer expr)

term :: (Fractional a, Real a) => Parser (Math a)
term = try (parens lexer expr)
    <|> try parseFunc
    <|> try parseConst
    <|> try parseFloatNum
    <|> try parseIntNum

table :: (Fractional a, Real a) => OperatorTable String () Identity (Math a)
table = [ [binary "^" (**) AssocLeft],
          [prefix "-" negate],
          [binary "*" (*) AssocLeft, binary "/" (/) AssocLeft ],
          [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft ]
        ]


expr :: (Fractional a, Real a) => Parser (Math a)
expr = buildExpressionParser table term


parseMath :: (Fractional a, Real a) => String -> Math a
parseMath input = case parse expr "Math" input of
    Left err  -> 0
    Right val -> val
