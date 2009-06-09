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
program = do
           constStart 
           op

constStart :: Parser EnvValue
constStart = do 
             string "["
             c <- consts
             string "]"
             return c

consts :: Parser EnvValue
consts = do 
         c <- Simple.Parse.const
         do 
           {
           try(string ",");
           rest <- consts;
           return (Env c rest);
           } 
           <|> 
           return (Env c End)
           
const :: Parser ConstValue
const = do 
        str <- many1 letter
        whiteSpace
        string "="
        whiteSpace
        value <- op
        return (Simple.Ast.Const str value)

whiteSpace :: Parser ()
whiteSpace = skipMany space

expr = zero_expr <|> succ_expr <|> parens op <|> op

opTest sign = 
      do {whiteSpace;
          try(string sign);
          whiteSpace}

table = [[tableOp (opTest "*") mul_expr AssocLeft],
         [tableOp (opTest "+") (plus_expr) AssocLeft]] where 
           tableOp s f assoc = Infix (do {s; return f}) assoc 

op = buildExpressionParser table expr
     

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

plus_expr e1 e2 = (Plus e1 e2)

mul_expr e1 e2 = (Mul e1 e2)