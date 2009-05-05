module Ocult.OcultParser where 

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Char
import Ocult.Ast

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x -> print x

ocultParser :: Parser [Rule String String]
ocultParser = do { 
                value <- chainl1 pattern ';' [R1 "err" "or"]
                ; char '.'
                ; return value               
                }

rule :: Parser (Rule String String)
rule = do {left <- pattern               
                 ;skipMany1 space
                 ;string "=>"
                 ;skipMany1 space
           ;right <- pattern
           ;return $ Rl left right
              }

pattern :: Parser (Pattern String String)
pattern =  do {name <- pattern
            ;skipMany1 space
            ;value1 <- pattern 
            ;skipMany1 space
            ;value2 <- pattern
            ;return $ PApp value1 value2
            }
            <|>     
         do {
             const <- many1 upper 
            ;return $ PConst const
            }
            <|>
         do {
            var <- many1 lower
            ;return $ PVar var
            }