module Simple.Parse where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Simple.Ast

import Control.Applicative hiding ((<|>), many)
import Data.Char (isSpace)
import List


parsePrg :: String -> Ast
parsePrg input =
    case parse program "" input of
      Left err -> error $ "Parser Error " ++ show err
      Right p -> p

program :: Parser Ast
program = op

whiteSpace :: Parser ()
whiteSpace = skipMany space

expr2 = zero_expr <|> succ_expr <|> parens op <|> op

opTest tegn = 
      do {whiteSpace;
          try(string tegn);
          whiteSpace}

table = [[tableOp (opTest "*") mul_expr AssocLeft],
         [tableOp (opTest "+") (plus_expr) AssocLeft]] where 
           tableOp s f assoc = Infix (do {s; return f}) assoc 

op = buildExpressionParser table expr2 


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
    e <- op
    whiteSpace
    return (Succ e)

{-plus_expr = do
  e1 <- expr
  whiteSpace
  string "+"
  whiteSpace
  e2 <- expr
  whiteSpace
  return (Plus e1 e2)
-}

plus_expr e1 e2 = (Plus e1 e2)

mul_expr e1 e2 = (Mul e1 e2)