{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Q.Parser where

import Math.Symbolic.Expression (Math(..))

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
import Data.Generics (Typeable, Data)


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

parseListMatch :: (Fractional a, Real a) => Parser (Math a)
parseListMatch = (\c -> Op "listM" [Sym . T.pack $ c])
             <$> (char '.' *> many1 (letter <|> digit <|> msymbol))


parseExprMatch :: (Fractional a, Real a) => Parser (Math a)
parseExprMatch = (\c -> Op "exprM" [Sym . T.pack $ 'a':c])
             <$> (char '_' *> char 'a' *> many1 (letter <|> digit <|> msymbol))

parseNumMatch :: (Fractional a, Real a) => Parser (Math a)
parseNumMatch = (\c -> Op "numM" [Sym . T.pack $ 'n':c])
            <$> (char '_' *> char 'n' *> many1 (letter <|> digit <|> msymbol))

parseSymMatch :: (Fractional a, Real a) => Parser (Math a)
parseSymMatch = (\c -> Op "symM" [Sym . T.pack $ 's':c])
            <$> (char '_' *> char 's' *> many1 (letter <|> digit <|> msymbol))


term :: (Fractional a, Real a) => Parser (Math a)
term = try (parens lexer expr)
    <|> try parseFunc
    <|> try parseListMatch
    <|> try parseExprMatch
    <|> try parseNumMatch
    <|> try parseSymMatch
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


parseExpr :: (Monad m, Fractional a, Real a, Typeable a, Data a) => (String, Int, Int) -> String -> m (Math a)
parseExpr (file, line, col) s =
    case runParser p () "" s of
        Left err  -> fail $ show err
        Right e   -> return e
    where
        p = do
            pos <- getPosition
            setPosition .
                flip setSourceName file .
                flip setSourceLine line .
                flip setSourceColumn col $
                pos
            spaces
            e <- expr
            eof
            return e
