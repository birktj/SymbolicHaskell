{-# LANGUAGE OverloadedStrings #-}
module Math.Symbolic.Display where


import Math.Symbolic.Expression

import Data.Word
import Data.Maybe
import Data.List
import Data.Ratio
import Data.String
import Data.Monoid
import Control.Arrow
import qualified Data.Text as T
import Data.Text (Text)


showMathAST :: (Show a) => Math a -> String
showMathAST (Sym x)     = T.unpack x
showMathAST (Numeric x) = show x
showMathAST (Op op xs)  = "[" <> T.unpack op <> " " <> intercalate ", " (showMathAST <$> xs) <> "]"
