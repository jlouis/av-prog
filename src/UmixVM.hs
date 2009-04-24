module UmixVM ()
where

foo :: String
foo = "wheee"


main :: IO ()
main = do
  f <- return foo
  putStrLn f
