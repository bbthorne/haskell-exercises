{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

scoreV (Score n) = n

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

instance Semigroup Score where
  (<>) = (+)

score :: Char -> Score
score c
  | cIn "aeioulnstr" = Score 1
  | cIn "dg"         = Score 2
  | cIn "bcmp"       = Score 3
  | cIn "fhvwy"      = Score 4
  | c == 'k'         = Score 5
  | cIn "jx"         = Score 8
  | cIn "qz"         = Score 10
  | otherwise        = Score 0
  where cIn = any (== c)

scoreString :: String -> Score
scoreString = foldr sumScore $ Score 0
  where sumScore c = (<> score c)
