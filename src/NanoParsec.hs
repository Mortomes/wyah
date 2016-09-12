module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative
import Data.Monoid

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser (Parser pf) s = case pf s of
  [(res, [])] -> res
  [(_, rs)] -> error "Parser did not consume entire stream"
  _ -> error "Parser error"


instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s ->
    fmap (\(a, s') -> (f a, s')) (cs s)

instance Applicative Parser where
  pure x = Parser $ \s -> pure (x,s)
  (Parser pf) <*> (Parser pa) = Parser $ \s -> do
    (f, s') <- pf s
    (a, s'') <- pa s'
    return (f a, s'')

instance Monad Parser where
  return = pure
  (Parser pa) >>= fpb = Parser $ \s -> do
    (a, s') <- pa s
    parse (fpb a) s'

instance MonadPlus Parser where
  mzero = Parser $ \cs -> []
  mplus (Parser pf) (Parser qf) = Parser $ \s -> pf s ++ qf s

instance Alternative Parser where
  empty = mzero
  (Parser pf)  <|> (Parser pg) = Parser $ \s ->
    case pf s of
      [] -> pg s
      res -> res


item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c,cs)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = do
  x <- item
  if pred x then return x else mzero

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl'` op) <|> return a

chainl' :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl'` op = do
  a <- p
  rest a
  where rest a = (do
                     f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n
