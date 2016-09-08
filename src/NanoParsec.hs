module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s = case parse m s of
  [(res, [])] -> res
  [(_, rs)] -> error "Parser did not consume entire stream"
  _ -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c:cs) -> [(c,cs)]

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s ->
    fmap (\(a, s') -> (f a, s')) (cs s)

instance Applicative Parser where
  pure x = Parser $ \s -> [(x, s)]
  (Parser lfab) <*> (Parser la) = Parser $ \s -> do
    (fab, s') <- lfab s
    (a, s'') <- la s'
    return (fab a, s'')

instance Monad Parser where
  return = pure
  (Parser sla) >>= fapslb = Parser $ \s -> do
    (a, s') <- sla s
    parse (fapslb a) s
