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

ocultParser :: Parser String
ocultParser = do {
              pattern ocultParser 
           --  pattern 'chainl1' ocultParser
              return "1"
              }
              <|> 
              do {
               char '.'
              ;return ""
              }
  
rule :: Parser Rule
rule = do {left <- pattern               
                 ;skipMany1 space
                 ;string "=>"
                 ;skipMany1 space
           ;right <- pattern
           ;return Rl left right
              }

pattern :: Parser Pattern
pattern =  do {
             toDo <- many1 string
            ;skipMany1 space
            ;value1 <- pattern 
            ;skipMany1 space
            ;value2 <- pattern
            ;return PApp value1 value2
            }
            <|>     
         do {
             const <- many1 upper 
            ;return PConst const
            }
            <|>
         do {
            var <- many1 lower
            ;return PVar var
            }