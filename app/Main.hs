module Main where

import Parser
import Lambda
import Control.Applicative
import Data.Maybe


-- Syntax:
-- M, N - lambda terms
-- "x" - variable
-- "(x.M)" - abstraction
-- "(M N)" - application
lambdaP :: Parser Expr
lambdaP = appP <|> absP <|> varP

varP :: Parser Expr
varP = Var <$> anyCharP

absP :: Parser Expr
absP = Abs <$> (charP '(' *> name) <*> lambdaP <* charP ')'
  where
        name = anyCharP <* charP '.'

appP :: Parser Expr
appP = App <$> l <* charP ' ' <*> r
  where
    l = charP '(' *> lambdaP
    r = lambdaP <* charP ')'

reduct :: String -> Maybe String
reduct s = show . reduction . snd <$> (runParser lambdaP s)

main :: IO ()
main = do
  putStrLn "Write lambda expressions like that \"(((a.(b.a)) c) d)\":"
  expr <- getLine
  putStrLn $ "Answer: " ++ (fromMaybe "No solution." $ reduct expr)
