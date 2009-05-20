module Ocult.OcultParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Ocult.Ast

import Control.Applicative hiding ((<|>), many)
import Data.Char (isSpace)
import List

runIO :: Show a => Parser a -> String -> IO ()
runIO p input =
    either (\err -> (putStr "parse error at " >>= (\_ -> print err)))
           (\parse -> print parse)
           (parse p "" input)

program :: String -> Program String String
program wholeProgram =
    let
      rules = split ";" wholeProgram
    in
      fmap buildRule rules

trim      :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

buildRule :: String -> (Rule String String)
buildRule rule  =
    let
      [pat, replaca] = split "=>" rule
      f :: String -> Pattern String String
      f i = case parse pattern "" (trim i) of
                  Left err -> error $ "Parser Error " ++ show err ++ (show (trim i))
                  Right p  -> p
    in
      Rl (f pat) (f replaca)

split :: String -> String -> [String]
split tok splitme = unfoldr (sp1 tok) splitme
    where sp1 _ "" = Nothing
          sp1 t s = case find (t `isSuffixOf`) (inits s) of
                      Nothing -> Just (s, "")
                      Just p -> Just (take ((length p) - (length t)) p,
                                      drop (length p) s)

--rule :: Parser ([(Pattern String String)])
--rule = do {       --    ;char ';'
        --   ;return [""] --value
       --   ;right <- auxRule
         -- ;left <- head value
         -- ;return left
--          ;right <- head (tail value)
--          ;return (Rl left (PConst "a"))
--          }

parsePrg :: String -> Program String String
parsePrg input =
    case parse prg "" input of
      Left err -> error $ "Parser Error " ++ show err
      Right p -> p

prg :: Parser (Program String String)
prg = many ruleline
  where
    ruleline =
        do rl <- rule
           string ";"
           return rl

rule :: Parser (Rule String String)
rule = do
  pat <- pattern
  string "=>"
  replaca <- pattern
  return $ Rl pat replaca

table = [[tableOp (skipMany space) PApp AssocLeft]] where
           tableOp s f assoc = Infix (do {s; return f}) assoc

pattern :: Parser (Pattern String String)
pattern = buildExpressionParser table patternInst

patternInst :: Parser (Pattern String String)
patternInst = patternVar <|> patternConst <|> parens patternInst

parens pattern =
    do
      try $ char '('
      pat <- try pattern
      char ')'
      return pat

patternConst :: Parser (Pattern String String)
patternConst = PConst <$> (try $ many1 letter)

patternVar :: Parser (Pattern String String)
patternVar = PVar <$> (try $ many1 lower)

