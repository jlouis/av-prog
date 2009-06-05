module Simple.Parse where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Simple.Ast

import Control.Applicative hiding ((<|>), many)
import Data.Char (isSpace)
import List

program :: Parser Ast
program = expr

whiteSpace :: Parser ()
whiteSpace = skipMany space

expr = zero_expr <|> succ_expr <|> plus_expr <|> parens expr

parens :: Parser Ast -> Parser Ast
parens exp = do
    string "("
    whiteSpace
    e <- exp
    whiteSpace
    string ")"
    whiteSpace
    return e

zero_expr = do
    string "z"
    whiteSpace
    return Zero

succ_expr = do
    string "s"
    whiteSpace
    e <- expr
    whiteSpace
    return (Succ e)

plus_expr = do
  e1 <- expr
  whiteSpace
  string "+"
  whiteSpace
  e2 <- expr
  whiteSpace
  return (Plus e1 e2)

