{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> (zeroOrMore . satisfy) isAlphaNum

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

-- S-expressions can begin and end with any number of spaces
-- 1. throw away leading and trailing spaces
-- 2. look for either an ident or an integer or
--    an open parenthesis followed by 1+ SExprs followed by a close parenthesis
parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseText <* spaces
  where parseText = (A <$> parseAtom) <|> (Comb <$> parseComb)
        parseAtom = (N <$> posInt) <|> (I <$> ident)
        parseComb = char '(' *> oneOrMore parseSExpr <* char ')'
