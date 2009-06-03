module Ocult.OcultParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Ocult.Ast

import Control.Applicative hiding ((<|>), many)
import Data.Char (isSpace)
import List

data ParserAst = A_App [ParserAst]
               | A_Const String
               | A_Var String
type ParserRule = ([ParserAst], [ParserAst])
type ParserPrg  = [ParserRule]

convertAst :: ParserAst -> Pattern String String
convertAst (A_Const s) = PConst s
convertAst (A_Var s)   = PVar s
convertAst (A_App [x]) = convertAst x
convertAst (A_App (x : r)) =
    PApp (convertAst x) (convertAst (A_App r))

convertRule :: ParserRule -> Rule String String
convertRule (a1, a2) =
    Rl (convertAst (A_App a1)) (convertAst (A_App a2))

convertPrg :: ParserPrg -> Program String String
convertPrg rules =
    fmap convertRule rules

parsePrg :: String -> Program String String
parsePrg input =
    case parse program "" input of
      Left err -> error $ "Parser Error " ++ show err
      Right p -> convertPrg p

whiteSpace =
    skipMany space

program :: Parser ParserPrg
program = many (do
                 whiteSpace
                 r <- rule;
                 string ";"
                 return r)

rule :: Parser ParserRule
rule = do
  p1 <- pattern
  string "=>"
  whiteSpace
  p2 <- pattern
  return (p1, p2)

pattern :: Parser [ParserAst]
pattern = do
    r <- many1 (do
                 p <- papp <|> pconst <|> pvar
                 whiteSpace
                 return p)
    whiteSpace
    return r

parens :: Parser a -> Parser a
parens p =
    do
      try $ char '('
      whiteSpace
      pat <- try p
      char ')'
      whiteSpace
      return pat

papp :: Parser ParserAst
papp = A_App <$> (try $ parens pattern)

pconst :: Parser ParserAst
pconst = A_Const <$> (try $ do
                        p <- many1 letter
                        whiteSpace
                        return p)

pvar :: Parser ParserAst
pvar = A_Var <$> (try $ do
                    p <- many1 lower
                    whiteSpace
                    return p)
