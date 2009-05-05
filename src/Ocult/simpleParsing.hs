module Main where 

import Text.tors.Parsec
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

ocultParser :: Parser [[String]]
ocultParser = do {
              pattern 'chainl1' ocultParser
              }
              <|> 
              do {
               char '.'
              ;return ""
              }
  
pattern :: Parser  [[String]]
pattern = do {left <- rule               
                 ;skipMany1 space
                 ;string "=>"
                 ;skipMany1 space
           ;right <- rule
           ;return ["rules", left, right]
              }

rule :: Parser [String]
rule =   do {rule <- many1 String
            skipMany1 space
            value1 <- many1 String 
            skipMany1 space
            value2 <- many1 String
            ;return ["rule", value1, value2]
            }
            <|>
         do {value <- many1 lower
            ;return value
            }
            <|>
            return ["error"]
              
term :: Parser String
term = do {
          first <- try{}
          }