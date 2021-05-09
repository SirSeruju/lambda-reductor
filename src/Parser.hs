module Parser where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      return (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input1, f) <- p1 input
      (input2, x) <- p2 input1
      return (input2, f x)

instance Alternative Parser where
  empty = Parser $ \input -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> (p1 input) <|> (p2 input)


predP :: (Char -> Bool) -> Parser Char
predP p = Parser f
  where
    f (x : xs)
      | p x       = Just (xs, x)
      | otherwise = Nothing
    f _ = Nothing

charP :: Char -> Parser Char
charP = predP . (==)

anyCharP :: Parser Char
anyCharP = Parser f
  where 
    f (x : xs) = Just (xs, x)
    f _        = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> Just $ swap $ span f input
  where swap (a, b) = (b, a)

stringP :: String -> Parser String
stringP = sequenceA . map charP
