module Ocult.OcultParser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Ocult.Ast
import List

runIO :: Show a => Parser a -> String -> IO ()
runIO p input =
    either (\err -> (putStr "parse error at " >>= (\_ -> print err)))
           (\parse -> print parse)
           (parse p "" input)

program :: String -> [(Rule String String)]
program wholeProgram =
    let
      rules = split ";" wholeProgram
    in
      fmap buildRule rules

buildRule :: String -> (Rule String String)
buildRule rule  =
    let
      [pat, replaca] = split "=>" rule
      f :: String -> Pattern String String
      f i = case parse pattern "" i of
                  Left err -> error $ "Parser Error " ++ show err
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

follows :: Parser String
follows = string "=>"

table = [[tableOp (skipMany space) PApp AssocLeft]] where
           tableOp s f assoc = Infix (do {s; return f}) assoc

pattern :: Parser (Pattern String String)
pattern = buildExpressionParser table patternInst

patternInst :: Parser (Pattern String String)
patternInst =
         do {var <- try $ many1 lower
            ;return $ PVar var
            }
         <|>
         do {const <- try $ many1 letter
            ;return $ PConst const
            }
         <|>
         do {try(char '(')
            ;pat <- try(pattern)
            ;char ')'
            ;return pat
           }