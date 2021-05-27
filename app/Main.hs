module Main where

import Lambda
import Text.ParserCombinators.Parsec


-- Syntax:
-- M, N - lambda terms
-- "x" - variable
-- "(x.M)" - abstraction
-- "(M N)" - application

--lambda = application <|> abstraction <|> variable
lambda = try variable <|> try abstraction <|> try application


variable = Var <$> letter

abstraction = Abs <$> (lb *> name) <* separator <*> (lambda <* rb)
  where
    lb = char '('
    rb = char ')'
    separator = char '.'
    name = letter

application = App <$> (lb *> lambda) <* separator <*> (lambda <* rb)
  where
    lb = char '('
    rb = char ')'
    separator = char ' '

reduct :: String -> Either ParseError String
reduct s =
  case parse lambda "" s of
    Left err -> Left err
    Right ex -> Right . show . reduction $ ex

main :: IO ()
main = do
  putStrLn "Write lambda expressions like that \"(((a.(b.a)) c) d)\":"
  expr <- getLine
  case reduct expr of
    Left err -> print err
    Right ex -> putStrLn $ "Answer:\n" ++ ex
