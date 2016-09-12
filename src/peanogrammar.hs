module PeanoGrammar where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Char
import Control.Applicative

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.nestedComments = True
  , Tok.identStart = letter
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames = reservedNames
  , Tok.reservedOpNames = reservedOps
  , Tok.caseSensitive = True
  }
